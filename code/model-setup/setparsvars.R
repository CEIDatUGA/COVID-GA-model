setparsvars <- function(est_these_pars, est_these_inivals, tint, n_knots)
{

  #this is a simple script that specifies model parameters, and variable names
  #it then assigns values for initial conditions and parameters
  #the idea is that there is a single place to specify them and will be loaded by other scripts
  
  #library(here)
  
  # State variables to track ------------------------------------------------
  varnames <- c("S", 
                "E1", "E2", "E3", "E4",  
                "Ia1", "Ia2", "Ia3", "Ia4", 
                "Isu1", "Isu2", "Isu3", "Isu4", 
                "Isd1", "Isd2", "Isd3", "Isd4", 
                "C1", "C2", "C3", "C4",  
                "H1", "H2", "H3", "H4",
                "C_new", "H_new", "D_new",
                "R",
                "D")
  
  #initial conditions are also considered parameters and some are estimated
  #we supply them on a log scale and in the code exponentiate to ensure no negative values
  inivals <- c(S_0 = 10600000, 
               E1_0 = log(40), #E2_0 = 40, E3_0 = 40, E4_0 = 40, 
               Ia1_0 = log(22), #Ia2_0 = 22, Ia3_0 = 22, Ia4_0 = 22, 
               Isu1_0 = log(90),# Isu2_0 = 90, Isu3_0 = 90, Isu4_0 = 90, 
               Isd1_0 = log(14), #Isd2_0 = 14, Isd3_0 = 14, Isd4_0 = 14, 
               C1_0 = 2, #C2_0 = 2, C3_0 = 2, C4_0 = 2, 
               H1_0 = 2, #H2_0 = 2, H3_0 = 2, H4_0 = 2, 
               R_0 = 0,
               D_0 = 0
  )
  
  inivalsraw <- c(S_0 = 10600000, 
               E1_0 = 40, #E2_0 = 40, E3_0 = 40, E4_0 = 40, 
               Ia1_0 = 22, #Ia2_0 = 22, Ia3_0 = 22, Ia4_0 = 22, 
               Isu1_0 = 90,# Isu2_0 = 90, Isu3_0 = 90, Isu4_0 = 90, 
               Isd1_0 = 14, #Isd2_0 = 14, Isd3_0 = 14, Isd4_0 = 14, 
               C1_0 = 2, #C2_0 = 2, C3_0 = 2, C4_0 = 2, 
               H1_0 = 2, #H2_0 = 2, H3_0 = 2, H4_0 = 2, 
               R_0 = 0,
               D_0 = 0
  )
  
  Ntot <- sum(inivalsraw)  # total population size - needed below

  
  # Parameters --------------------------------------------------------------
  #rev_logistic <- function(x) {
  #  log((1/x)-1)
  #}
  
  #note that a lot of parameters below are transformed versions of what the meaning specifies
  #see the Google sheet for detailed definitions and explanations
  parvals <- c(log_beta_s = log(0.45/Ntot), #rate of infection of symptomatic 
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
               
               # log_g_e = rev_logistic(1/4), #rate of movement through E/Ia/Isu/Isd/C/H compartments
               # log_g_a = rev_logistic(1/3.5),
               # log_g_su = rev_logistic(1/6),
               # log_g_sd = rev_logistic(1/3),
               # log_g_c = rev_logistic(1/3),  
               # log_g_h = rev_logistic(1/12),
               
               log_max_diag = log(1), #max for factor by which movement through Isd happens faster (quicker diagnosis) 
               log_diag_inc_rate = log(10), #rate at which faster diagnosis ramps up to max
               log_half_diag = log(tint),  #time at which intervention is at 50%
               
               max_detect_par = log(1),  #max fraction detected
               log_detect_inc_rate = log(10), #speed at which fraction detected ramps up
               log_half_detect = log(tint), #time at which intervention is at 50%
               
               frac_asym = 1.5, #fraction asymptomatic
               frac_hosp = 2, #fraction diagnosed that go into hospital, modeled as 1/(1+exp(frac_hosp))
               frac_dead = 1.2, #fraction hospitalized that die
               log_theta_cases = log(10),
               log_theta_hosps = log(10),
               log_theta_deaths = log(10),
               log_sigma_dw = log(0.1)
  )
  
  knot_coefs <- rep(0, n_knots)
  names(knot_coefs) <- paste0("b", 1:n_knots)
  parvals = c(parvals, knot_coefs) 
  
  parnames = names(parvals)
  
  #all parameter values, including initial conditions
  allparvals = c(parvals,inivals)
  
  # select if all parameters and initial values are being estimated
  # if the est_these_pars vector is null, the default is to esitmate all
  if (est_these_pars[1] == "all")
  {
    params_to_estimate <- names(parvals) 
  } else {
    params_to_estimate <- est_these_pars
  }
  
  if (est_these_inivals[1] == "all")
  {
    inivals_to_estimate <- names(inivals)
  } else {
    inivals_to_estimate <- est_these_inivals
  }
  
    
  # Create an output list to save -------------------------------------------
  
  par_var_list = list()
  par_var_list$params_to_estimate =  params_to_estimate
  par_var_list$inivals_to_estimate =  inivals_to_estimate
  par_var_list$varnames = varnames
  par_var_list$parnames = parnames
  par_var_list$allparvals = allparvals

  return(par_var_list)
} #finish function that defines and writes variables and parameters
