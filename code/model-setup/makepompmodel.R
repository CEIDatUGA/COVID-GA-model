makepompmodel <- function(par_var_list, pomp_data, covar_table, n_knots)
{

  # This generates a pomp object for an SEIR model of COVID 19. 
  # The pomp object can be used for simulating trajectories and fitting.
  #
  # Authors: John Drake
  #          Andreas Handel
  #          Andrew Tredennick
  
  # Load libraries ----------------------------------------------------------
   library(dplyr)
   library(pomp)
  
  ############################################################################
  # Code to define  process model -------------------------------------------
  ############################################################################
  
  # 1-step function of model
  pomp_step <- Csnippet(
      "
    // C indexes at 0, for R users making things 1 bigger and start 
    // with index 1, i.e. leave trans[0] empty, same for rate[0]
    
    double rate[15];  // 14 rates plus 1 empty
    double trans[30];  // 29 transitions plus 1 empty
    
    double E_tot, Ia_tot, Isu_tot, Isd_tot, C_tot, H_tot;
    double foi;  // force of infection
    double g_sd, g_c;  // rate of transition through I_sd compartments
    double detect_frac, diag_speedup; // fraction of those that get eventually diagnosed
    double beta;
    double dW;  // environmental stochasticity/noise
  
    E_tot = E1+E2+E3+E4;  // all pre-symptomatic
    Ia_tot = Ia1+Ia2+Ia3+Ia4;  // all asymptomatic
    Isu_tot = Isu1+Isu2+Isu3+Isu4;  // all symptomatic who will remain undiagnosed
    Isd_tot = Isd1+Isd2+Isd3+Isd4;  // all symptomatic who will become diagnosed
    C_tot = C1+C2+C3+C4;  // all diagnosed/cases
    H_tot = H1+H2+H3+H4;  // all hospitalized
    
    // compute the environmental stochasticity
    dW = rgammawn(exp(log_sigma_dw), dt);
    
    
    // ---------------------------------------------------------------
    // below we define and compute different quantities that
    // depend on time-varying interventions  
    // ---------------------------------------------------------------
    
    // Force of Infection (foi):
    // Each group can have its own transmission rate.
    // Symptomatic are assumed to transmit the most, other groups have reduced 
    //    transmission by some factor.
    // All parameters are used in the model such that they can take on any 
    //    numeric value (useful for fitting).
    // The overall foi is modulated by the unacast data stream as covariate.
    
    //foi = rel_beta_change*( pow( ( 1/(1+exp(-5.65)) ), t ) ) * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
    //foi = rel_beta_change * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
    beta = rel_beta_change * exp(log_beta_s + dot_product(K, &b1, &seas_1));
    foi = beta * (Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot);
    
    
    // Time-dependent rate of movement through Isd dummy compartments.
    // Starts at no speedup, then increases with time up to a max.
    // Ramp-up speed, time at which half-max is reached and max value are fitted.
    
    diag_speedup = (1 + exp(log_max_diag) ) *  pow(t, exp(log_diag_inc_rate)) / (pow(exp(log_half_diag),exp(log_diag_inc_rate))  + pow(t, exp(log_diag_inc_rate)));
    g_sd = diag_speedup*exp(log_g_sd); //shortened time in symptomatic stage prior to diagnosis
    g_c = exp(log_g_c)/diag_speedup; //increased time in symptomatic stage post diagnosis
    
    
    // Time dependent fraction of those that move into detected category at the 
    //    end of the E phase.
    // Starts at 0 at simulation start, then ramps up to some max value (0-1). 
    // Ramp-up speed and max value are fitted.
    
    detect_frac = 1/(1+exp(max_detect_par)) * pow(t, exp(log_detect_inc_rate))  / ( pow(exp(log_half_detect),exp(log_detect_inc_rate)) + pow(t,exp(log_detect_inc_rate)));
    
    
    // -----------------------------------
    // Compute the transition rates
    // -----------------------------------
    rate[1] = foi * dW/dt;  //infection, movement from S to E
    rate[2] = exp(log_g_e);  //movement through E compartments
    rate[3] = exp(log_g_e) * 1/(1+exp(frac_asym));  //from E to Ia
    rate[4] = exp(log_g_e) * (1 - 1/(1+exp(frac_asym))) * (1 - detect_frac);  //from E to Isu
    rate[5] = exp(log_g_e) * (1 - 1/(1+exp(frac_asym))) * detect_frac;  //from E to Isd
  
    rate[6] = exp(log_g_a);  //movement through Ia stages
    rate[7] = exp(log_g_su);  //movement through Isu stages
    rate[8] = g_sd;  //movement through Isd stages - computed above
    rate[9] = g_c;  //movement through C stages - computed above
    
    rate[10] = g_c * 1/(1+exp(frac_hosp));  //movement from C to H  
    rate[11] = g_c * (1 - 1/(1+exp(frac_hosp)));  //movement from C to R  
  
    rate[12] = exp(log_g_h);  //movement through H stages  
    rate[13] = exp(log_g_h) *  1/(1+exp(frac_dead));  //movement from H to D  
    rate[14] = exp(log_g_h) * (1 - 1/(1+exp(frac_dead)));  //movement from H to R  
    
    
    // ------------------------------------------
    // Compute the state transitions
    // ------------------------------------------
    // Some explanations on code, especially parts with multiple transitions.
    // reulermultinom(3, E4, &rate[3], dt, &trans[5]) says there are 3 potential 
    //    movements that come out of the E4 number of individuals, following 
    //    rates rate[3], rate[4], and rate[5]. dt is the time interval. We assign
    //    the number of transitions to trans[5], trans[6], and trans[7]. 
    //    Those trans are then used to balance the equations starting on line xxx.
    // The & symbol in C code creates automatic pointers to that iterate through 
    //    rate and trans starting at the value in the index (i) to i+(3-1).
    // m = 3, a positive integer, is number of potential transitions 
    //    (to Ia, Isu, Isd).
    // size = E4, a positive integer, is the number of individuals at 
    //    risk (pop size in E4).
    // rate is a pointer to the vector of transition (death) rates 
    //    (here rate[3:5] but in C form with the &).
    // dt, a positive real number, is the duration of time interval.
    // trans is a pointer to the vector that will hold the random deviate 
    //    (here trans[5:7] but in C form with the &).
    // See also here: https://kingaa.github.io/pomp/vignettes/C_API.html
    
    reulermultinom(1, S, &rate[1], dt, &trans[1]); // infection
    
    reulermultinom(1, E1, &rate[2], dt, &trans[2]); // move through E stages
    reulermultinom(1, E2, &rate[2], dt, &trans[3]);
    reulermultinom(1, E3, &rate[2], dt, &trans[4]);
    reulermultinom(3, E4, &rate[3], dt, &trans[5]); // move from E to Ia or Isu 
                                                    // or Isd, goes through rates 
                                                    // 3-5 and trans 5-7
    
    reulermultinom(1, Ia1, &rate[6], dt, &trans[8]); // move through Ia stages
    reulermultinom(1, Ia2, &rate[6], dt, &trans[9]);
    reulermultinom(1, Ia3, &rate[6], dt, &trans[10]);
    reulermultinom(1, Ia4, &rate[6], dt, &trans[11]); // move into R
  
    reulermultinom(1, Isu1, &rate[7], dt, &trans[12]); // move through Isu stages
    reulermultinom(1, Isu2, &rate[7], dt, &trans[13]);
    reulermultinom(1, Isu3, &rate[7], dt, &trans[14]);
    reulermultinom(1, Isu4, &rate[7], dt, &trans[15]); // move into R
  
    reulermultinom(1, Isd1, &rate[8], dt, &trans[16]); // move through Isd stages
    reulermultinom(1, Isd2, &rate[8], dt, &trans[17]);
    reulermultinom(1, Isd3, &rate[8], dt, &trans[18]);
    reulermultinom(1, Isd4, &rate[8], dt, &trans[19]); // move into C
    
    reulermultinom(1, C1, &rate[9], dt, &trans[20]); // move through C stages
    reulermultinom(1, C2, &rate[9], dt, &trans[21]);
    reulermultinom(1, C3, &rate[9], dt, &trans[22]);
    reulermultinom(2, C4, &rate[10], dt, &trans[23]); // move from C to either H 
                                                      // or R, goes through rates
                                                      // 10-11 and trans 23-24
  
  
    reulermultinom(1, H1, &rate[12], dt, &trans[25]); // move through H stages
    reulermultinom(1, H2, &rate[12], dt, &trans[26]);
    reulermultinom(1, H3, &rate[12], dt, &trans[27]);
    reulermultinom(2, H4, &rate[13], dt, &trans[28]); // move from H to either D 
                                                      // or R, goes through rates
                                                      // 13-14 and trans 28-29
  
    // ----------------------------------------------------------------
    // Apply transitions to state variables (balance equations)
    // -----------------------------------------------------------------
    S -= trans[1];
  
    E1 += trans[1] - trans[2];
    E2 += trans[2] - trans[3];
    E3 += trans[3] - trans[4];
    E4 += trans[4] - trans[5] - trans[6] - trans[7];
  
    Ia1 += trans[5] - trans[8]; //from E
    Ia2 += trans[8] - trans[9];
    Ia3 += trans[9] - trans[10];
    Ia4 += trans[10] - trans[11]; //into R
  
    Isu1 += trans[6] - trans[12]; //from E
    Isu2 += trans[12] - trans[13];
    Isu3 += trans[13] - trans[14];
    Isu4 += trans[14] - trans[15]; //into R
  
    Isd1 += trans[7] - trans[16]; //from E
    Isd2 += trans[16] - trans[17];
    Isd3 += trans[17] - trans[18];
    Isd4 += trans[18] - trans[19]; //into C
  
    C1 += trans[19] - trans[20]; //from Isd4
    C2 += trans[20] - trans[21];
    C3 += trans[21] - trans[22];
    C4 += trans[22] - trans[23] - trans[24]; //into H or R
    C_new += trans[19]; // new cases tracker, reset at obs times
  
    H1 += trans[23] - trans[25]; //from C
    H2 += trans[25] - trans[26];
    H3 += trans[26] - trans[27];
    H4 += trans[27] - trans[28] - trans[29]; //into D or R
    H_new += trans[23];  // new hosps tracker, reset at obs times
  
    R += trans[11] + trans[15] + trans[24] + trans[29];
    D += trans[28];
    D_new += trans[28];  // new deaths tracker, reset at obs times
    "
  )
  
  
  # C snippet for initial condition specification ---------------------------
  
  rinit <- Csnippet(
    "
    S = nearbyint(S_0);
    E1 = nearbyint(exp(E1_0));
    E2 = nearbyint(exp(E1_0));
    E3 = nearbyint(exp(E1_0));
    E4 = nearbyint(exp(E1_0));
    
    Ia1 = nearbyint(exp(Ia1_0));
    Ia2 = nearbyint(exp(Ia1_0));
    Ia3 = nearbyint(exp(Ia1_0));
    Ia4 = nearbyint(exp(Ia1_0));
    
    Isu1 = nearbyint(exp(Isu1_0));
    Isu2 = nearbyint(exp(Isu1_0));
    Isu3 = nearbyint(exp(Isu1_0));
    Isu4 = nearbyint(exp(Isu1_0));
  
    Isd1 = nearbyint(exp(Isd1_0));
    Isd2 = nearbyint(exp(Isd1_0));
    Isd3 = nearbyint(exp(Isd1_0));
    Isd4 = nearbyint(exp(Isd1_0));
    
    C1 = nearbyint(C1_0);
    C2 = nearbyint(C1_0);
    C3 = nearbyint(C1_0);
    C4 = nearbyint(C1_0);
    C_new = nearbyint(C1_0);
  
    H1 = nearbyint(H1_0);
    H2 = nearbyint(H1_0);
    H3 = nearbyint(H1_0);
    H4 = nearbyint(H1_0);
    H_new = nearbyint(H1_0);
  
    R = nearbyint(R_0);
    D = nearbyint(D_0);
    D_new = nearbyint(D_0);
    "
  )
  
  
  ############################################################################
  # Code to define estimation components of  model 
  ############################################################################
  
  # Define likelihood function ----------------------------------------------
  
  # Details on dnbinom_mu:
  # Question: Is there official documentation on this function somewhere? Can't find it. 
  # dnbinom_mu is negative binomial parameterized by mu (see ?rnbinom)
  # Given data (e.g. x = cases) and model-predicted value (e.g. size = C_new) 
  #    and mean (e.g theta1), estimate probability.
  # the value 1 in the last slot corresponds to log = 1.
  
  
  # R code: dbinom(x=reports, size=H, prob=rho, log=log)
  # C code: lik = dbinom(reports,H,rho,give_log)
  
  dmeas <- Csnippet(
    "
    double d1, d2, d3;
    double theta1, theta2, theta3;
    theta1 = exp(log_theta_cases);
    theta2 = exp(log_theta_hosps);
    theta3 = exp(log_theta_deaths);
    
    if(ISNA(cases)) {
      d1 = 0;  // loglik is 0 if no observations
    } else {
      d1 = dnbinom_mu(cases, theta1, C_new, 1); 
    }
    
    if(ISNA(hosps)) {
      d2 = 0;  // loglik is 0 if no observations
    } else {
      d2 = dnbinom_mu(hosps, theta2, H_new, 1);
    }
    
    if(ISNA(deaths)) {
      d3 = 0;  // loglik is 0 if no observations
    } else {
      d3 = dnbinom_mu(deaths, theta3, D_new, 1);
    }
    
    lik = d1 + d2 + d3;  // sum the individual likelihoods
    lik = (give_log) ? lik : exp(lik);  // return loglik or exp(lik)
    "
  )
  
  
  # Define process simulator for observations  ------------------------------
  # given the model states (C_new, H_new, D_new)
  # produce simulated data/obervations based on the specified distribution
  # rnbinom_mu is negative binomial parameterized by mu (see ?rnbinom)
  # Given model-predicted value (e.g. size = C_new) and mean (e.g theta1), 
  #    generate a single random value for expected observed cases (i.e. n=1).
  # size/C_new is number of trials, the outcome is expected number of successes, 
  #    given the specified mean
  # Some examples from pomp website
  #R code: reports = rbinom(n=1, size=H, prob=rho), C code: reports = rbinom(H,rho)
  
  rmeas <- Csnippet(
    "
    double theta1, theta2, theta3;
    theta1 = exp(log_theta_cases);
    theta2 = exp(log_theta_hosps);
    theta3 = exp(log_theta_deaths);
    cases = rnbinom_mu(theta1, C_new);  // for forecasting 
    hosps = rnbinom_mu(theta2, H_new);  // for forecasting
    deaths = rnbinom_mu(theta3, D_new);  // for forecasting
    "
  )
  
  
  
  ############################################################################
  # assign values for model parameters and initial conditions -----------------
  ############################################################################
  allparvals <- par_var_list$allparvals
  params_to_estimate <- par_var_list$params_to_estimate
  inivals_to_estimate <- par_var_list$inivals_to_estimate
  varnames <- par_var_list$varnames
  allparnames <- names(allparvals)  # includes initial conditions
  
  # remove any column in data that's not time or the fitted variables
  # otherwise pomp might get indigestion
  dat_for_pomp <- pomp_data %>% select(time, cases, hosps, deaths)
  
  
  # Define the pomp model object --------------------------------------------
  # the last line with the cdir statement is to try and prevent error messages 
  #    doing parallel computation on windows. 
  #See here: https://kingaa.github.io/sbied/mif/mif.html#a-local-search-of-the-likelihood-surface
  pomp_model <- pomp(
    data = dat_for_pomp, 
    times = "time",
    t0 = 1,  # set first sim time to first observation time
    # covar = covariate_table(covar_table, times = "time", order = "constant"),
    covar = covar, 
    dmeasure = dmeas,
    rmeasure = rmeas,
    rinit = rinit,
    rprocess = euler(step.fun = pomp_step, delta.t = 1/20),
    statenames = varnames,
    paramnames = allparnames, 
    obsnames = c("cases", "hosps", "deaths"),
    accumvars = c("C_new", "H_new", "D_new"),    
    globals = paste0("int K = ", as.character(n_knots), ";"),
    cdir=".",
    cfile="tmp1" 
  )
  
  return(pomp_model)

}
