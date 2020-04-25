

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
    # maxval <- 1
    covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
    covars <- as.data.frame(covars) %>%
      mutate(time = 1:n()) %>%
      rename("rel_beta_change" = covars)
    covars$rel_beta_change <- covar_no_action
    
    
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
    
  } else if(covar_action == "lowest_sd") {
    covars <- pomp_model@covar@table
    minval <- min(covars)
    id <- which.min(covars)
    news <- seq(minval, 0.3, length.out = 7)
    covars[id:(id+7-1)] <- news
    covars[(id+7):length(covars)] <- 0.3
    covars <- c(covars, rep(0.3, times = horizon))
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
    
    # browser()
    
    
    last_time <- obs_sim %>%
      filter(time == max(time)) %>%
      dplyr::select(.id, cases, hosps, deaths)
    dat <- last_time %>%
      filter(.id == "data")
    init_id <- last_time %>%
      filter(.id != "data") %>%
      mutate(obs_cases = dat$cases,
             obs_hosps = dat$hosps,
             obs_deaths = dat$deaths) %>%
      mutate(dif1 = (obs_cases - cases)^2,
             dif2 = (obs_hosps - hosps)^2,
             dif3 = (obs_deaths - deaths)^2) %>%
      group_by(.id) %>%
      mutate(totdif = mean(c(dif1, dif2, dif3))) %>%
      ungroup() %>%
      filter(totdif == min(totdif)) %>%
      pull(.id) 
    
    inits <- obs_sim %>%
      filter(.id == init_id) %>%
      tail(1) %>%
      dplyr::select(-time, -.id, -cases, -hosps, -deaths, -rel_beta_change) %>%
      summarise(S_0=S,
                E1_0=log(E1), 
                Ia1_0=log(Ia1), 
                Isu1_0=log(Isu1), 
                Isd1_0=log(Isd1),
                C1_0 = C1, 
                H1_0 = H1,
                R_0=R, D_0 = D)
    
    param_vals[which(names(param_vals) %in% names(inits))] <- inits
    
    
    # Update pomp covariate table
    if(covar_action == "status_quo") {
      covars <- pomp_model@covar@table
      covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
    }
    
    if(covar_action == "more_sd") {
      covars <- pomp_model@covar@table
      lastval <- as.numeric(tail(t(covars), 1))
      # minval <- min(covars)
      minval <- 0.3  # max observed in NY
      dec <- seq(lastval, minval, length.out = 7)
      final <- rep(minval, times = (horizon - length(dec)))
      covars <- c(covars, dec, final)
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
    }
    
    if(covar_action == "less_sd") {
      covars <- pomp_model@covar@table
      lastval <- as.numeric(tail(t(covars), 1))
      maxval <- 0.8
      inc <- seq(lastval, maxval, length.out = 7)
      final <- rep(maxval, times = (horizon - length(inc)))
      covars <- c(covars, inc, final)
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
    }
    
    if(covar_action == "normal") {
      covars <- pomp_model@covar@table
      lastval <- as.numeric(tail(t(covars), 1))
      maxval <- 1
      inc <- seq(lastval, maxval, length.out = 7)
      final <- rep(maxval, times = (horizon - length(inc)))
      covars <- c(covars, inc, final)
      covars <- as.data.frame(covars) %>%
        mutate(time = 1:n()) %>%
        rename("rel_beta_change" = covars)
    }
    
    
    
    # Update the pomp model with new covariates
    M2 <- pomp_model
    time(M2) <- max(time(pomp_model))+seq_len(horizon)
    timezero(M2) <- max(time(pomp_model))
    newcovars <- covars %>%
      tail(horizon+1)
    M2@covar <- covariate_table(newcovars, times = "time", order = "constant")
    
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
    sims <- sim_out %>% 
      left_join(dates_df, by = "time") %>%
      mutate(Period = "Future")
    
    
    fits <- obs_sim %>%
      dplyr::select(-rel_beta_change) %>%
      left_join(dates_df, by = "time") %>%
      filter(.id != "data") %>%
      gather(key = "Variable", value = "Value", -Date, -time, -.id) %>%
      group_by(Date, time, Variable) %>%
      summarise(ptvalue = ceiling(quantile(Value, 0.5))) %>%
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