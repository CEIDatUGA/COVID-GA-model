

simulate_trajectories <- function(
  pomp_model,
  start_date = "2020-03-01",
  covar_action = "status_quo",
  covar_no_action = NULL,
  param_vals, 
  forecast_horizon_wks = 6,
  nsims = 100,
  obs_sim) {
  
  # Number of days to project into the future
  horizon <- 7*forecast_horizon_wks # length of time (days to extend covariate)
  

  if(covar_action == "no_intervention") {
    covars <- pomp_model@covar@table
    covars <- c(covars[1,], rep(as.numeric(tail(covars[1,], 1)), times = horizon))
    covars <- as.data.frame(covars) %>%
      mutate(time = 1:n()) %>%
      rename("rel_beta_change" = covars)
    covars$rel_beta_change <- covar_no_action
    covars$trend_sim <- 100  # this equals 1 after logistic transform
    
    covar = covariate_table(
      t = covars$time,
      seas = bspline.basis(
        x=t,
        nbasis=n_knots,
        degree=3
      ),
      rel_beta_change = as.matrix(covars$rel_beta_change),
      trend_sim = as.matrix(covars$trend_sim),
      fit = 0,
      times="t",
      order = "constant"
    )
    
    
    # Update the pomp model with new covariates
    newtimes <- c(time(pomp_model), max(time(pomp_model))+seq_len(horizon))
    M2 <- pomp(
      pomp_model,
      time = newtimes, # update time of pomp object 
      covar = covar
    )
    
    # Run the simulations
    sim_out <- pomp::simulate(M2, 
                              params = param_vals,
                              nsim = nsims, 
                              format="data.frame")
    
    
    # pomp runs with internal time units, add real time to results
    end_date <- as.Date(start_date) + max(sim_out$time) - 1
    dates <- seq.Date(as.Date(start_date), end_date, "days") 
    dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
    
    # Combine pomp simulations with dates
    sims_ret <- sim_out %>% 
      left_join(dates_df, by = "time") %>%
      mutate(Period = ifelse(Date > Sys.Date(), "Future", "Past"))
    
  } else if(covar_action == "lowest_sd") {
    covars <- pomp_model@covar@table
    minval <- min(covars["rel_beta_change", ])
    id <- which.min(covars["rel_beta_change", ])
    news <- seq(minval, 0.3, length.out = 7)
    sdv <- covars["rel_beta_change", ]
    sdv[id:(id+7-1)] <- news
    sdv[(id+7):length(sdv)] <- 0.3
    sdv <- c(sdv, rep(0.3, times = horizon))
    
    covars <- as.data.frame(covars) %>%
      mutate(time = 1:n()) %>%
      rename("rel_beta_change" = covars)
    
    # Update the pomp model with new covariates
    newtimes <- c(time(pomp_model), max(time(pomp_model))+seq_len(horizon))
    M2 <- pomp(
      pomp_model,
      time = newtimes, # update time of pomp object 
      covar = covariate_table(covars, 
                              times = "time",
                              order = "constant") # update covariate
    )
    
    # Run the simulations
    sim_out <- pomp::simulate(M2, 
                              params = param_vals,
                              nsim = nsims, 
                              format="data.frame")
    
    # pomp runs with internal time units, add real time to results
    end_date <- as.Date(start_date) + max(sim_out$time) - 1
    dates <- seq.Date(as.Date(start_date), end_date, "days") 
    dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
    
    # Combine pomp simulations with dates
    sims_ret <- sim_out %>% 
      left_join(dates_df, by = "time") %>%
      mutate(Period = ifelse(Date > Sys.Date(), "Future", "Past"))
  } else {
    
    last_time <- obs_sim %>%
      filter(time == max(time)) %>%
      dplyr::select(.id, mle_id, cases, hosps, deaths)
    dat <- last_time %>%
      filter(.id == "data")
    init_id <- last_time %>%
      filter(.id != "data") %>%
      mutate(obs_cases = dat$cases,
             obs_hosps = dat$hosps,
             obs_deaths = dat$deaths) %>%
      mutate(dif1 = abs(obs_cases - cases) / obs_cases,
             dif3 = abs(obs_deaths - deaths) / obs_deaths) %>%
      group_by(.id, mle_id) %>%
      mutate(totdif = sum(c(dif1, dif3), na.rm = TRUE)) %>%
      ungroup() %>%
      filter(totdif == min(totdif)) %>%
      dplyr::select(.id, mle_id)
    
    trend <- obs_sim %>%
      filter(mle_id == init_id$mle_id) %>%
      filter(.id == init_id$.id) %>%
      pull(trendO)
    
    inits <- obs_sim %>%
      filter(mle_id == init_id$mle_id) %>%
      filter(.id == init_id$.id) %>%
      tail(1) %>%
      dplyr::select(-time, -.id, -cases, -hosps, -deaths, -rel_beta_change) %>%
      summarise(S_0=round(mean(S)),
                E1_0=log(round(mean(E1))), 
                Ia1_0=log(round(mean(Ia1))), 
                Isu1_0=log(round(mean(Isu1))), 
                Isd1_0=log(round(mean(Isd1))),
                C1_0 = round(mean(C1)), 
                H1_0 = round(mean(H1)),
                R_0=round(mean(R)),
                D_0 = round(mean(D)),
                trendO_0 = mean(trendO))
    
    param_vals[which(names(param_vals) %in% names(inits))] <- inits
    
    
    # Update pomp covariate table
    if(covar_action == "status_quo") {
      covars <- pomp_model@covar@table["rel_beta_change", ]
      covars <- c(covars, rep(as.numeric(tail(covars, 1)), times = horizon))
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
      trend_sim <- rep(mean(tail(trend, 30)), nrow(covars))
    }
    
    if(covar_action == "more_sd") {
      covars <- pomp_model@covar@table["rel_beta_change", ]
      lastval <- as.numeric(tail(covars, 1))
      # minval <- min(covars)
      minval <- 0.3  # max observed in NY
      dec <- seq(lastval, minval, length.out = 7)
      final <- rep(minval, times = (horizon - length(dec)))
      covars <- c(covars, dec, final)
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
      trend_sim <- rep(mean(tail(trend, 30)), nrow(covars))
    }
    
    if(covar_action == "less_sd") {
      covars <- pomp_model@covar@table["rel_beta_change", ]
      lastval <- as.numeric(tail(covars, 1))
      maxval <- 0.8
      inc <- seq(lastval, maxval, length.out = 14)
      final <- rep(maxval, times = (horizon - length(inc)))
      covars <- c(covars, inc, final)
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
      trend_sim <- rep(mean(tail(trend, 30)), nrow(covars))
    }
    
    if(covar_action == "normal") {
      covars <- pomp_model@covar@table[1,]
      lastval <- as.numeric(tail(covars, 1))
      maxval <- 1
      inc <- seq(lastval, maxval, length.out = 14)
      final <- rep(maxval, times = (horizon - length(inc)))
      covars <- c(covars, inc, final)
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
      # trend_inc <- seq(inits$trendO_0, 100, length.out = 7)
      # trend_inc <- rep(inits$trendO_0, times = horizon)
      # trend_sim <- c(inits$trendO_0, trend_inc)
      trend_sim <- rep(mean(tail(trend, 30)), nrow(covars))
    }
    
    
    # Update the pomp model with new covariates
    M2 <- pomp_model
    time(M2) <- max(time(pomp_model))+seq_len(horizon)
    timezero(M2) <- max(time(pomp_model))
    newcovars <- covars %>%
      tail(horizon+1)
    newtrend <- trend_sim %>%
      tail(horizon+1)
    covar = covariate_table(
      t = c(timezero(M2), time(M2)),
      seas = bspline.basis(
        x=t,
        nbasis=n_knots,
        degree=3
      ),
      rel_beta_change = as.matrix(newcovars$rel_beta_change),
      trend_sim = as.matrix(newtrend),
      fit = 0,
      times="t",
      order = "constant"
    )
    
    # Update the globals string to indicate this is a 
    # simulation run with a set trend_sim covariate, as
    # specified.
    M2@covar <- covar
    
    # Run the simulations
    sim_out <- pomp::simulate(M2, 
                              params = param_vals,
                              nsim = nsims, 
                              format="data.frame")
    # par(mfrow = c(2,1))
    # plot(c(pomp_data %>% filter(Variable == "Acases") %>% pull(Value), sim_out$C_new), type = "l")
    # plot(c(pomp_data %>% filter(Variable == "Cdeaths") %>% pull(Value), sim_out$D_new), type = "l")
    
    # pomp runs with internal time units, add real time to results
    end_date <- as.Date(start_date) + max(sim_out$time) - 1
    dates <- seq.Date(as.Date(start_date), end_date, "days") 
    dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
    
    # Combine pomp simulations with dates
    sims <- sim_out %>% 
      left_join(dates_df, by = "time") %>%
      mutate(Period = "Future")
    
    
    fits <- obs_sim %>%
      # filter(mle_id == init_id$mle_id) %>%
      # filter(.id == init_id$.id) %>%
      dplyr::select(-rel_beta_change) %>%
      left_join(dates_df, by = "time") %>%
      filter(.id != "data") %>%
      gather(key = "Variable", value = "Value", -Date, -time, -.id) %>%
      group_by(Date, time, Variable) %>%
      summarise(ptvalue = ceiling(mean(Value))) %>%
      ungroup() %>%
      spread(Variable, ptvalue)
    
    # calib_out <- obs_sim %>%
    #   filter(.id == init_id) %>%
    #   left_join(dates_df, by = "time") %>%
    #   mutate(Period = "Past")
    calib_rep <- tibble()
    for(i in 1:length(unique(sims$.id))) {
      tmp <- fits %>%
        mutate(.id = as.character(i)) %>%
        mutate(Period = "Past")
      calib_rep <- bind_rows(calib_rep, tmp)
    }
    
    sims_ret <- sims %>%
      mutate(.id = as.character(.id)) %>%
      bind_rows(calib_rep) %>%
      arrange(.id, Date)
  }
  
  return(list(sims_ret=sims_ret, covars=covars))
}