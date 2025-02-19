# runscenarios.R

runscenarios <- function(mif_res, pomp_model, pomp_data, filename_label)
{

  # Load libraries ----------------------------------------------------------
  
  library(pomp)
  library(tidyverse)
  library(here)
  
  #this is the fixed part for every figure, will be combined with figure labels below
  figname = here('output/figures',filename_label)
  
  scenario_sim_res = list() #this will contain all results created and returned by this function
  
  mifs = mif_res$mif_runs
  pfs = mif_res$pf_runs
  
  n_ini_cond = length(pfs)
  ll = list()
  for (i in 1:n_ini_cond)
  {
    ll1 <- sapply(pfs[[i]], logLik)
    ll[[i]] <- logmeanexp(ll1, se = TRUE)
  }
  
  # get estimated values for all parameters that were estimated for each run 
  mif_coefs <- data.frame(matrix(unlist(sapply(mifs, coef)), 
                                 nrow = length(mifs), 
                                 byrow = T))
  colnames(mif_coefs) <- names(coef(mifs[[1]]))  # names are the same for all mifs
  
  # convert the list containing the log likelihoods for 
  # each run stored in ll into a data frame
  ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T))
  
  # combine the ll_df and mif_coefs data frames. 
  # Also do some cleaning/renaming
  pf_logliks <- ll_df %>%
    dplyr::rename("LogLik" = X1,
                  "LogLik_SE" = X2) %>%
    dplyr::mutate(MIF_ID = 1:n()) %>%
    dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
    bind_cols(mif_coefs) %>%
    dplyr::arrange(-LogLik)
  
  all_mles <- pf_logliks %>%
    filter(LogLik > (max(LogLik)-2)) %>%
    dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)
  
  
  obs_sim <- simulate(pomp_model, 
                      params = all_mles[1, ],
                      nsim = 1000, 
                      format="data.frame",
                      include.data = TRUE)
  
  # Run simulations ---------------------------------------------------------
  weeks_ahead <- 6
  num_sims <- 100
  
  out_sims <- tibble()
  for(i in 1:nrow(all_mles)){
  # for(i in 1:1){
    print(sprintf('starting to simulate for MIF run %d',i))
    mles <- all_mles[i, ]
    
    sim_sq <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                    covar_action = "status_quo", param_vals = mles,
                                    forecast_horizon_wks = weeks_ahead, 
                                    nsims = num_sims, obs_sim = obs_sim) %>%
      mutate(SimType = "status_quo")
    
    sim_na <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                    covar_action = "no_intervention", 
                                    covar_no_action = 1,
                                    param_vals = mles,
                                    forecast_horizon_wks = weeks_ahead,
                                    nsims = num_sims, obs_sim = obs_sim) %>%
      mutate(SimType = "no_intervention") %>%
      mutate(.id = as.character(.id))
    
    sim_minsd <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                       covar_action = "lowest_sd", 
                                       param_vals = mles,
                                       forecast_horizon_wks = weeks_ahead,
                                       nsims = num_sims, obs_sim = obs_sim) %>%
      mutate(SimType = "lowest_sd") %>%
      mutate(.id = as.character(.id))
    
    sim_msd <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                    covar_action = "more_sd",
                                    param_vals = mles, 
                                    forecast_horizon_wks = weeks_ahead,
                                    nsims = num_sims, obs_sim = obs_sim) %>%
      mutate(SimType = "linear_increase_sd")
    
    sim_lsd <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                    covar_action = "less_sd",
                                    param_vals = mles, 
                                    forecast_horizon_wks = weeks_ahead,
                                    nsims = num_sims, obs_sim = obs_sim) %>%
      mutate(SimType = "linear_decrease_sd")
    
    sim_nor <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                     covar_action = "normal",
                                     param_vals = mles, 
                                     forecast_horizon_wks = weeks_ahead,
                                     nsims = num_sims, obs_sim = obs_sim) %>%
      mutate(SimType = "return_normal")
    
    all_sims <- bind_rows(sim_sq, sim_na, sim_minsd, sim_msd, sim_lsd, sim_nor) %>%
      mutate(mle_id = i,
             rep_id =  paste(.id, mle_id, sep = "-"))
    out_sims <- bind_rows(out_sims, all_sims)
  }
  
  # Save the simulations
  # not used, instead returned to calling function, could be saved there
  #fname <- paste0("output/simulation-scenarios/simulation-scenarios-", Sys.Date(), ".rds")
  #saveRDS(object = out_sims, file = here(fname))

  scenario_sim_res$out_sims = out_sims
    
  
  # out_sims %>%
  #   filter(SimType == "return_normal") %>%
  #   filter(.id == "1") %>%
  #   filter(mle_id == "1") %>%
  #   pull(S) %>% plot()
    
  sim_summs <- out_sims %>%
    dplyr::select(SimType, Period, Date, cases, hosps, deaths) %>%
     rename("Acases" = cases,
            "Bhosps" = hosps,
            "Cdeaths" = deaths) %>%
    gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
    group_by(SimType, Period, Date, Variable) %>%
    summarise(lower = ceiling(quantile(Value, 0.1)),
              ptvalue = ceiling(quantile(Value, 0.5)),
              upper = ceiling(quantile(Value, 0.9))) %>%
     ungroup()
   
  
  cumulative_summs <- out_sims %>%
     dplyr::select(SimType, Date, cases, hosps, deaths, rep_id) %>%
     rename("Acases" = cases,
            "Bhosps" = hosps,
            "Cdeaths" = deaths) %>%
     gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
     group_by(SimType, Variable, rep_id) %>%
     mutate(Value = cumsum(Value)) %>%
     group_by(SimType, Variable, Date) %>%
     summarise(min = quantile(Value, 0.1),
               ptvalue = ceiling(quantile(Value, 0.5)),
               max = quantile(Value, 0.9)) %>%
     ungroup() %>%
     filter(Date == max(Date)) %>%
     mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
            SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
            SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
            SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
            SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
            SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
     mutate(SimType = SimType2) %>%
     dplyr::select(-SimType2)
  
  
  # Make a data data frame for plotting -------------------------------------
  
  #filename <- tail(list.files(path = here("data/"), pattern = "us-ct-clean"), 1)
  #fullpath <- paste0("data/", filename)
  
  pomp_data <- pomp_data %>%
    dplyr::select(-Location, -time) %>%
    rename("Acases" = cases,
           "Bhosps" = hosps,
           "Cdeaths" = deaths) %>%
    gather(key = "Variable", value = "Value", -Date) %>%
    mutate(SimType = "obs", Period = "Past")
  
  
  
  # Make the plots ----------------------------------------------------------
  
  mycols <- c("#a11c3e", "#5798d1", "#252525", "#319045",
              "#5e2b7b", "#e2908c", "#226e83")
  
  variable_names <- c(
    "Acases" = 'New cases',
    "Bhosps" ='New hospitalizations',
    "Cdeaths" = 'New deaths'
  )
  
  variable_names_cum <- c(
    "Acases" = 'Total cases',
    "Bhosps" ='Total hospitalizations',
    "Cdeaths" = 'Total deaths'
  )
  
  # Fits to data
  end_date <- as.Date("2020-03-01") + max(obs_sim$time) - 1
  dates <- seq.Date(as.Date("2020-03-01"), end_date, "days") 
  dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
  
  fits <- obs_sim %>%
    left_join(dates_df, by = "time") %>%
    filter(.id != "data") %>%
    dplyr::select(Date, cases, hosps, deaths) %>%
    rename("Acases" = cases,
           "Bhosps" = hosps,
           "Cdeaths" = deaths) %>%
    gather(key = "Variable", value = "Value", -Date) %>%
    group_by(Date, Variable) %>%
    summarise(lower = ceiling(quantile(Value, 0.025)),
              ptvalue = ceiling(quantile(Value, 0.5)),
              upper = ceiling(quantile(Value, 0.975))) %>%
    ungroup()
  
  pl1 <- ggplot(fits, aes(x = Date)) +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(aes(y = ptvalue)) +
    geom_point(data = pomp_data, aes(x = Date, y = Value)) +
    facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma)+
    theme_minimal()
  
  scenario_sim_res$fitplot = pl1
  
  ggsave(filename = paste0(figname,"fits-to-data.png"), width = 8.5, height = 3, 
         units = "in", dpi = 300)
  
  browser()
  
  # Cumulative min/maxes 6 weeks out
  scen_labs <- c("1. Increased social distancing",
                 "2. Status quo",
                 "3. Relax social distancing",
                 "4. Return to normal",
                 "5. Continuously improving social distancing",
                 "6. No intervention")
  title <- paste("Range of projections by", max(cumulative_summs$Date))
  ggplot(cumulative_summs, aes(x = SimType, color = SimType)) +
    geom_segment(aes(xend = SimType, y = min, yend = max), size = 3) +
    geom_point(aes(y=ptvalue), color = "white", size = 1) +
    facet_wrap(~Variable) +
    # scale_colour_viridis_d(end = 0.8) +
    scale_color_manual(values = mycols) +
    facet_wrap(~Variable, ncol = 3, scales = "free_x", labeller = labeller(Variable = variable_names_cum)) +
    ylab("Number of persons") +
    xlab("") +
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = scen_labs) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(color = FALSE) +
    coord_flip() +
    ggtitle(title, subtitle = "scenarios are described above") 
  ggsave(filename = here("output/figures/cumulative-forecasts.png"), width = 8.5, height = 4, 
         units = "in", dpi = 300)
  
  ggplot(cumulative_summs, aes(x = SimType, color = SimType)) +
    geom_segment(aes(xend = SimType, y = min, yend = max), size = 3) +
    geom_point(aes(y=ptvalue), color = "white", size = 1) +
    facet_wrap(~Variable) +
    # scale_colour_viridis_d(end = 0.8) +
    scale_color_manual(values = mycols) +
    facet_wrap(~Variable, ncol = 3, scales = "free_x", labeller = labeller(Variable = variable_names_cum)) +
    ylab("Number of persons") +
    xlab("") +
    scale_y_continuous(labels = scales::comma, trans = "log", 
                       limits = c(1000,10000000), breaks = c(1000,10000,100000,1000000))+
    scale_x_discrete(labels = scen_labs) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(color = FALSE) +
    coord_flip() +
    ggtitle(title, subtitle = "scenarios are described above")
  ggsave(filename = here("output/figures/cumulative-forecasts-log.png"), width = 8.5, height = 4, 
         units = "in", dpi = 300)
  
  
  all_summs <- sim_summs %>%
    mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "3Relax social distancing", SimType),
           SimType2 = ifelse(SimType == "no_intervention", "6No intervention", SimType2),
           SimType2 = ifelse(SimType == "lowest_sd", "5Continuously improving social distancing", SimType2),
           SimType2 = ifelse(SimType == "status_quo", "2Status quo", SimType2),
           SimType2 = ifelse(SimType == "linear_increase_sd", "1Increased social distancing", SimType2),
           SimType2 = ifelse(SimType == "return_normal", "4Return to normal", SimType2)) %>%
    mutate(SimType = SimType2) %>%
    dplyr::select(-SimType2) %>%
    mutate(Period = ifelse(Period == "Past", "APast", "BFuture"))
  
  labs <- c("1. Increased social distancing",
            "2. Status quo",
            "3. Relax social distancing",
            "4. Return to normal",
            "5. Continuously improving social distancing",
            "6. No intervention")
  # All scenarios - line, natural
  ggplot(all_summs, aes(x = Date, color = SimType)) +
    geom_line(aes(y = ptvalue)) +
    geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
    facet_wrap(~Variable, ncol = 3, scales = "free_y", 
               labeller = labeller(Variable = variable_names)) +
    scale_color_manual(values = mycols, name = "", labels= labs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(legend.position = "top")
  ggsave("./output/figures/all-projs-line-nat.png", width = 8.5, height = 4, 
         units = "in", dpi = 300)
  
  
  # All scenarios - line, log
  ggplot(all_summs, aes(x = Date,  color = SimType)) +
    geom_line(aes(y = ptvalue)) +
    geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
    facet_wrap(~Variable, ncol = 3, scales = "free_y", 
               labeller = labeller(Variable = variable_names)) +
    scale_color_manual(values = mycols, name = "", labels= labs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma, trans = "log", 
                       limits = c(1,100000), breaks = c(10,100,1000,10000,100000))+
    theme_minimal() +
    theme(legend.position = "top")
  ggsave("./output/figures/all-projs-line-log.png", width = 8.5, height = 4, 
         units = "in", dpi = 300)
  
  
  
  scen_labs <- c("1Increased social distancing" = "1. Increased\nsocial distancing",
                 "2Status quo" = "2. Status quo",
                 "3Relax social distancing" = "3. Relax\nsocial distancing",
                 "4Return to normal" = "4. Return to normal",
                 "5Continuously improving social distancing" = "5. Continuously\nimproving\nsocial distancing",
                 "6No intervention" = "6. No intervention")
  
  ## NATURAL SCALE
  # Cases
  pomp_data <- pomp_data %>%
    mutate(Period = "APast")
  collabs <- c("Past", "Future")
  ggplot(all_summs %>%
           filter(Variable == "Acases"),
         aes(x = Date, color = Period, fill = Period)) +
    geom_point(data = pomp_data %>%
                 filter(Variable == "Acases") %>%
                 dplyr::select(-SimType),
               aes(x = Date, y = Value), size = 1, color = "grey35") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(aes(y = ptvalue)) +
    facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
    scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
    scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma)+
    theme_minimal() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("New daily cases") +
    coord_cartesian(ylim = c(0, 30000))
  ggsave("./output/figures/cases-trajs-nat.png", width = 8.5, height = 3, 
         units = "in", dpi = 300)
  
  # Hosps
  ggplot(all_summs %>%
           filter(Variable == "Bhosps"),
         aes(x = Date, color = Period, fill = Period)) +
    geom_point(data = pomp_data %>%
                 filter(Variable == "Bhosps") %>%
                 dplyr::select(-SimType),
               aes(x = Date, y = Value), size = 1, color = "grey35") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(aes(y = ptvalue)) +
    facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
    scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
    scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma)+
    theme_minimal() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("New daily hospitalizations") +
    coord_cartesian(ylim = c(0, 10000))
  ggsave("./output/figures/hosps-trajs-nat.png", width = 8.5, height = 3, 
         units = "in", dpi = 300)
  
  # Deaths
  ggplot(all_summs %>%
           filter(Variable == "Cdeaths"),
         aes(x = Date, color = Period, fill = Period)) +
    geom_point(data = pomp_data %>%
                 filter(Variable == "Cdeaths") %>%
                 dplyr::select(-SimType),
               aes(x = Date, y = Value), size = 1, color = "grey35") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(aes(y = ptvalue)) +
    facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
    scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
    scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma)+
    theme_minimal() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("New daily deaths") +
    coord_cartesian(ylim = c(0, 6000))
  ggsave("./output/figures/deaths-trajs-nat.png", width = 8.5, height = 3, 
         units = "in", dpi = 300)
  
  
  ## LOG SCALE
  # Cases
  ggplot(all_summs %>%
           filter(Variable == "Acases"),
         aes(x = Date, color = Period, fill = Period)) +
    geom_point(data = pomp_data %>%
                 filter(Variable == "Acases") %>%
                 dplyr::select(-SimType),
               aes(x = Date, y = Value), size = 1, color = "grey35") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(aes(y = ptvalue)) +
    facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
    scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
    scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma, trans = "log", 
                       limits = c(1,200000), breaks = c(10,100,1000,10000,100000)) +
    theme_minimal() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("New daily cases")
  ggsave("./output/figures/cases-trajs-log.png", width = 8.5, height = 3, 
         units = "in", dpi = 300)
  
  # Hosps
  ggplot(all_summs %>%
           filter(Variable == "Bhosps"),
         aes(x = Date, color = Period, fill = Period)) +
    geom_point(data = pomp_data %>%
                 filter(Variable == "Bhosps") %>%
                 dplyr::select(-SimType),
               aes(x = Date, y = Value), size = 1, color = "grey35") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(aes(y = ptvalue)) +
    facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
    scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
    scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma, trans = "log", 
                       limits = c(1,100000), breaks = c(10,100,1000,10000,100000)) +
    theme_minimal() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("New daily hospitalizations")
  ggsave("./output/figures/hosps-trajs-log.png", width = 8.5, height = 3, 
         units = "in", dpi = 300)
  
  # Deaths
  ggplot(all_summs %>%
           filter(Variable == "Cdeaths"),
         aes(x = Date, color = Period, fill = Period)) +
    geom_point(data = pomp_data %>%
                 filter(Variable == "Cdeaths") %>%
                 dplyr::select(-SimType),
               aes(x = Date, y = Value), size = 1, color = "grey35") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(aes(y = ptvalue)) +
    facet_grid(~SimType, labeller = labeller(SimType = scen_labs)) +
    scale_color_brewer(type = "qual", name = NULL, labels = collabs) +
    scale_fill_brewer(type = "qual", name = NULL, labels = collabs) +
    theme_minimal() +
    ylab("Number of persons") +
    scale_y_continuous(labels = scales::comma, trans = "log", 
                       limits = c(1,20000), breaks = c(10,100,1000,10000,100000)) +
    theme_minimal() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("New daily deaths")
  ggsave("./output/figures/deaths-trajs-log.png", width = 8.5, height = 3, 
         units = "in", dpi = 300)
  
  
  
  
  # Increase social distancing trajectory
  # ggplot(sim_summs %>%
  #          filter(SimType == "linear_increase_sd"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   scale_y_continuous(labels = scales::comma, trans = "log", 
  #                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  #   theme_minimal() +
  #   ggtitle("1. Increased social distancing")
  # ggsave("./output/figures/increased-sd-traj-log.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # # sim_summs %>%
  # #   filter(SimType == "linear_increase_sd") %>%
  # #   filter(Period == "Future") %>%
  # #   filter(Date == min(Date))
  # # sim_summs %>%
  # #   filter(SimType == "linear_increase_sd") %>%
  # #   filter(Period == "Past") %>%
  # #   filter(Date == max(Date))
  # 
  # ggplot(sim_summs %>%
  #          filter(SimType == "linear_increase_sd"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   scale_y_continuous(labels = scales::comma)+
  #   theme_minimal() +
  #   ggtitle("1. Increased social distancing")
  # ggsave("./output/figures/increased-sd-traj.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # # Status quo trajectory
  # ggplot(sim_summs %>%
  #          filter(SimType == "status_quo"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma, trans = "log", 
  #                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  #   ggtitle("2. Status quo")
  # ggsave("./output/figures/status-quo-traj-log.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # ggplot(sim_summs %>%
  #          filter(SimType == "status_quo"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma)+
  #   ggtitle("2. Status quo")
  # ggsave("./output/figures/status-quo-traj.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # # Relax social distancing trajectory
  # ggplot(sim_summs %>%
  #          filter(SimType == "linear_decrease_sd"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma, trans = "log", 
  #                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  #   ggtitle("3. Relax social distancing")
  # ggsave("./output/figures/relax-sd-traj-log.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # ggplot(sim_summs %>%
  #          filter(SimType == "linear_decrease_sd"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma)+
  #   ggtitle("3. Relax social distancing")
  # ggsave("./output/figures/relax-sd-traj.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # # Return to normal
  # ggplot(sim_summs %>%
  #          filter(SimType == "return_normal"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma, trans = "log", 
  #                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  #   ggtitle("4. Return to normal")
  # ggsave("./output/figures/return-normal-traj-log.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # ggplot(sim_summs %>%
  #          filter(SimType == "return_normal"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma)+
  #   ggtitle("4. Return to normal")
  # ggsave("./output/figures/return-normal-traj.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # # No intervention
  # ggplot(sim_summs %>%
  #          filter(SimType == "no_intervention"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma, trans = "log", 
  #                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  #   ggtitle("6. No intervention")
  # ggsave("./output/figures/no-intervention-traj-log.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # ggplot(sim_summs %>%
  #          filter(SimType == "no_intervention"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma)+
  #   ggtitle("6. No intervention")
  # ggsave("./output/figures/no-intervention-traj.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # # Best SD to date
  # ggplot(sim_summs %>%
  #          filter(SimType == "lowest_sd"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma, trans = "log", 
  #                      limits = c(1,200000), breaks = c(10,100,1000,10000,100000))+
  #   ggtitle("5. Continuously improving social distancing")
  # ggsave("./output/figures/lowest-sd-traj-log.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # ggplot(sim_summs %>%
  #          filter(SimType == "lowest_sd"), 
  #        aes(x = Date, color = Period, fill = Period)) +
  #   geom_point(data = pomp_data, aes(x = Date, y = Value), size = 1, color = "grey35") +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  #   geom_line(aes(y = ptvalue)) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", labeller = labeller(Variable = variable_names)) +
  #   scale_color_brewer(type = "qual") +
  #   scale_fill_brewer(type = "qual") +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   theme_minimal() +
  #   scale_y_continuous(labels = scales::comma)+
  #   ggtitle("5. Continuously improving social distancing")
  # ggsave("./output/figures/lowest-sd-traj.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  
  
  # nada <- sim_summs %>%
  #   filter(SimType == "no_intervention") %>%
  #   filter(Variable == "cases")
  # 
  # par(mfrow = c(1,2))
  # plot(cumsum(ptvalue)~Date, data = nada, type = "h", ylab = "Cumulative cases")
  # plot(ptvalue~Date, data = nada, type = "l", ylab = "New cases")
  
  
  # rel_beta_change = seq(0.1, 2, by = 0.01)
  # log_beta_s <- -16.9
  # Isd_tot = 14*4
  # Isu_tot = 90*4
  # E_tot = 40*4
  # Ia_tot = 22*4
  # C_tot = 2*4
  # H_tot = 2*4
  # trans_e = 2
  # trans_a = 0
  # trans_c = 1
  # trans_h = 10
  # foi = rel_beta_change * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
  # plot(foi)
  
  
  # all_cumms <- all_summs %>%
  #   dplyr::select(SimType, Variable, Date, ptvalue) %>%
  #   group_by(SimType, Variable) %>%
  #   mutate(total = cumsum(ptvalue)) %>%
  #   ungroup()
  # 
  # ggplot(all_cumms, aes(x = Date, y = total, color = SimType)) +
  #   geom_line(size = 1) +
  #   facet_wrap(~Variable, scales = "free_y")
  # 
  # test <- all_summs %>%
  #   filter(Variable == "Acases") %>%
  #   filter(SimType == "5No intervention")
  # par(mfrow=c(1,2))
  # plot(test$ptvalue)
  # plot(cumsum(test$ptvalue))
  
  
  # All scenarios, ribbon nat
  # ggplot(all_summs, aes(x = Date,  fill = SimType)) +
  #   geom_line(aes(y = ptvalue, alpha = Period)) +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  #   geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", 
  #              labeller = labeller(Variable = variable_names)) +
  #   scale_fill_viridis_d(end = 0.8, name = "", labels= labs) +
  #   scale_alpha_manual(values = c(1,0)) +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   scale_y_continuous(labels = scales::comma)+
  #   theme_minimal()
  # ggsave("./output/figures/all-projs-ribbon-nat.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
  # 
  # # All scenarios, ribbon log
  # ggplot(all_summs, aes(x = Date,  fill = SimType)) +
  #   geom_line(aes(y = ptvalue, alpha = Period)) +
  #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  #   geom_vline(aes(xintercept = Sys.Date()), color = "grey35", linetype = 2) +
  #   facet_wrap(~Variable, ncol = 3, scales = "free_y", 
  #              labeller = labeller(Variable = variable_names)) +
  #   scale_fill_viridis_d(end = 0.8, name = "", labels= labs) +
  #   scale_alpha_manual(values = c(1,0)) +
  #   theme_minimal() +
  #   ylab("Number of persons") +
  #   scale_y_continuous(labels = scales::comma, trans = "log", 
  #                      limits = c(1,100000), breaks = c(10,100,1000,10000,100000))+
  #   theme_minimal()
  # ggsave("./output/figures/all-projs-ribbon-log.png", width = 8.5, height = 3, 
  #        units = "in", dpi = 300)
}