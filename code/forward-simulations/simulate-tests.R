# simulate-pomp-model.R
  #
  # This script runs the previously generated pomp model
  # it does not do fitting, but can be used for exploration 
  # and to generate generate synthetic data
  
  #rm(list = ls(all.names = TRUE))
  
  # Load libraries ----------------------------------------------------------
  library(pomp)
  library(dplyr)
  library(tidyr)
  
  parsource = "base"
  
  # Load the pomp object ----------------------------------------------------
  filename <- here('output/pomp-model.RDS')
  pomp_model <- readRDS(filename)
  
  # load values for model parameters and initial conditions -----------------
  filename = here('output/var-par-definitions.RDS')
  par_var_list <- readRDS(filename) 
  allparvals <- par_var_list$allparvals
  params_to_estimate = par_var_list$params_to_estimate
  inivals_to_estimate = par_var_list$inivals_to_estimate
  
  #load data
  #this data was used to generate the pomp model
  #pomp somehow destroys any non-numeric data, so need to get date back from here
  filename <- here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))
  pomp_data <- readRDS(filename)
  
  if (parsource == "base" )
  {
    simparvals = allparvals #this runs the simulation with the default parameter settings
  }
  if (parsource == "mif")
  {
    # load table of best fit estimates produced by mif fitting ----------------------------------------------------
    filename = here('output/tables/par-table.RDS')
    mif_result_df = readRDS(filename)
    new_pars = mif_result_df[1,-(1:3)] #these are the fitted parameters, first 3 are ID and LL/SE
    newparvals = allparvals
    newparvals[names(new_pars)] = as.numeric(new_pars) #update parameter values based on best fit
    simparvals = newparvals #this runs the simulation with the parameter settings obtained from the MIF with the lowest LL
  }
  if (parsource == "pmcmc")
  {
    #needs to be written
  }
  if (parsource == "manual") #manually specify parameter values, includes initial conditions
  {
    simparvals <- c(log_beta_s = log(0.5/10600000), #rate of infection of symptomatic 
                    trans_e = 2, 
                    trans_a = 0, 
                    trans_c = 1,  
                    trans_h = 10,  #parameter that determines relative infectiousness of E/Ia/C classes compared to Isu/Isd 
                    
                    log_g_e = log(4/4), #rate of movement through E/Ia/Isu/Isd/C/H compartments
                    log_g_a = log(4/3.5),
                    log_g_su = log(4/6),
                    log_g_sd = log(4/3),
                    log_g_c = log(4/3),
                    log_g_h = log(4/12),
                    
                    log_max_diag = -10, #max for factor by which movement through Isd happens faster (quicker diagnosis), modeled as (1 + exp(log_max_diag) ) 
                    log_diag_inc_rate = log(10), #rate at which faster diagnosis ramps up to max
                    log_half_diag = log(12),  #time at which intervention is at 50%
                    
                    max_detect_par = -0,  #max fraction detected, modeled as 1/(1+exp(max_detect_par))
                    log_detect_inc_rate = log(10), #speed at which fraction detected ramps up
                    log_half_detect = log(12), #time at which intervention is at 50%
                    
                    frac_asym = 1.5, #fraction asymptomatic
                    frac_hosp = 3, #fraction diagnosed that go into hospital
                    frac_dead = 1.2, #fraction hospitalized that die
                    log_theta_cases = log(10),
                    log_theta_hosps = log(10),
                    log_theta_deaths = log(10),
                    log_sigma_dw = log(0.1),
                    
                    S_0 = 10600000, 
                    E1_0 = log(40), #E2_0 = 40, E3_0 = 40, E4_0 = 40, 
                    Ia1_0 = log(22), #Ia2_0 = 22, Ia3_0 = 22, Ia4_0 = 22, 
                    Isu1_0 = log(90),# Isu2_0 = 90, Isu3_0 = 90, Isu4_0 = 90, 
                    Isd1_0 = log(14), #Isd2_0 = 14, Isd3_0 = 14, Isd4_0 = 14, 
                    C1_0 = 2, #C2_0 = 2, C3_0 = 2, C4_0 = 2, 
                    H1_0 = 2, #H2_0 = 2, H3_0 = 2, H4_0 = 2, 
                    R_0 = 0,
                    D_0 = 0
    )
  } #end if to specify parameter values manually
  
  
  # extend pomp model to include information beyond the last data point
  horizon <- 7*1 #length of time (days to extend covariate)
  # we need to supply any covariates into the future
  covars <- pomp_model@covar@table
  covars <- c(covars, rep(as.numeric(tail(t(covars), 1)), times = horizon))
  covars <- as.data.frame(covars) %>%
    mutate(time = 1:n()) %>%
    rename("rel_beta_change" = covars)
  # covars$rel_beta_change <- 1
  
  
  #update pomp model
  M2 <- pomp(pomp_model, 
             time = c(time(pomp_model), max(time(pomp_model))+seq_len(horizon)), #update time of pomp object 
             covar = covariate_table(covars, times = "time", order = "constant"), #update covariate
  )
  
  #run simulation a number of times
  #for some weird reason, setting data=true creates lots of error messages
  sim_out <- pomp::simulate(M2, 
                            params=simparvals, #set parameter values to be used for simulation 
                            nsim=50, format="data.frame")
  
  # pomp runs with internal time units, add real time to results
  start_date <- pomp_data$Date[1]
  end_date <- start_date + max(sim_out$time) - 1
  dates <- seq.Date(start_date, end_date, "days") 
  dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
  
  #combine pomp simulations with dates
  sims <- sim_out %>% left_join(dates_df)

