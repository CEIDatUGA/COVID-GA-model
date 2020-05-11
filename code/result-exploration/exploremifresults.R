# exploremifresults.R
# This function takes results produced by run-mif for exploration/plotting

exploremifresults <- function(mif_res, n_knots)
{
  
  #  ---------------------------------------------------------
  #To-do ideas
  #Correlations of parameters across MIF iterations
  #bivariate histograms of likelihoods/posteriors
  #  ---------------------------------------------------------
  
  res_list=list() #this will contain all results produced here and returned 

  # Load libraries ----------------------------------------------------------
  library(dplyr)
  library(pomp)
  library(purrr)
  library(ggplot2)

  # pull out individual pieces from mif_res super-list ----------------------------------------------------
  # this is a list of mif objects for each initial condition 
  # followed by pfilter objects run a specified number of times after each mif is run
  mifs = mif_res$mif_runs
  pfs = mif_res$pf_runs

  pomp_model = mif_res$pomp_model
  pomp_data = mif_res$pomp_data   
    
  ############################################################################
  # take values for model parameters and initial conditions -----------------
  ############################################################################
  par_var_list = mif_res$par_var_list
  allparvals <- par_var_list$allparvals
  params_to_estimate = par_var_list$params_to_estimate
  inivals_to_estimate = par_var_list$inivals_to_estimate
  varnames <- par_var_list$varnames
  allparnames <- names(allparvals) #includes initial conditions
  
  # ---------------------------------------------------------
  #Traceplots of the mif iterations
  # ---------------------------------------------------------
  # take list of mifs, get traces, merge into a data frame with a column for mif_run
  mif_df <- mifs %>% purrr::map(traces) %>% purrr::map(melt) %>% dplyr::bind_rows( .id = "mif_run") 
  
  #only keep those params/variables that are estimated  
  mif_df <- mif_df %>% filter(variable %in% tidyselect::all_of(c(params_to_estimate,inivals_to_estimate,"loglik")))
  
  #browser()
  
  #make a plot of traces for all mif runs  
  pl1 <- mif_df %>% ggplot(aes(x=iteration,y=value,group= mif_run, color=factor(variable)))+
    geom_line()+
    guides(color=FALSE)+
    facet_wrap(~variable,scales="free_y")+
    theme_bw()
  
  res_list$traceplot = pl1 #save trace plot to list
  
  # ---------------------------------------------------------
  # Make a data frame that contains best fit parameter estimates for each mif run, 
  # as well as the mean likelihood. The latter comes from the pfilter run at the end of each mif run
  # ---------------------------------------------------------
  
  # for each mif run, take the pf runs and compute mean log likelihood
  n_ini_cond = length(mifs)
  ll = list()
  for (i in 1:n_ini_cond) #do last part not in parallel
  {
    ll1 <- sapply(pfs[[i]], logLik)
    ll[[i]] <- logmeanexp(ll1, se = TRUE)
  }
  # convert the list containing the log likelihoods for 
  # each run stored in ll into a data frame
  ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T))
  
  # extract best fit paramter values for each mif run
  coef_est_df <- data.frame(matrix(unlist(sapply(mifs, coef)), 
                                 nrow = length(mifs), 
                                 byrow = T))
  colnames(coef_est_df) <- names(coef(mifs[[1]]))  
  
  #only keep those parameters that are being estimated
  # coef_est_df = select(coef_est_df, params_to_estimate, inivals_to_estimate)
  coef_est_df = select(coef_est_df, params_to_estimate)
  
  # combine the ll_df and coef_est_df data frames. 
  # Also do some cleaning/renaming
  mif_result_df <- ll_df %>%
    dplyr::rename("LogLik" = X1,
                  "LogLik_SE" = X2) %>%
    dplyr::mutate(MIF_ID = 1:n()) %>%
    dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
    bind_cols(coef_est_df) %>%
    dplyr::arrange(-LogLik)
  
  res_list$partable = mif_result_df
  
  #print(mif_result_df)
  #filename = here('output/tables/par-table.RDS')
  #saveRDS(mif_result_df,filename)
  
  
  
  # ---------------------------------------------------------
  # make a second data frame that stores parameters and estimates in biologically meaningful units
  # by applying the transformations done to each parameter 
  # (see the rprocess bit of the pomp model code for how each parameter is used in the model)
  # ---------------------------------------------------------
  
  # function that does the transformation
  transform_params <- function(param_df, param_trans) 
  {
    out <- param_df #fill with new values
    for (j in 1:nrow(param_df))
    {
    for(i in 1:ncol(param_df)) 
      {
      
      trans <- param_trans[i]
      x <- param_df[j,i]
      if(trans == "log") { #changes log-parameters to natural scale
        out[j,i] <- exp(x)
      } 
      if(trans == "logis")
      {
        out[j,i] <- 1/(1+exp(x))
      }
      if(trans == "logplus")
      {
        out[j,i] <- 1+exp(x)
      }
      if(trans == "loginv") #exponentiates log rates, turns them into time. also multiply by 4 for 4 compartments
      {
        out[j,i] <- 4/exp(x)
      }
      if(trans == "lin") #no transform
      {
        out[j,i] <- x
      }
    }
    }
    return(out)
  } 
  
  # specify transformation for each parameter to get it into biologically meaningful units
  
  #this needs to be in the same order as the parameters listed in allparvals 
  param_trans <- c("log", 
                   "logis", "logis", "logis", "logis", #trans
                   "loginv", "loginv", "loginv", "loginv", "loginv", "loginv", #gi
                   "logplus", "log", "log", #diag
                   "logis", "log", "log",  #detect
                   "logis", "logis", "logis", #frac
                   "log", "log", "log", #theta
                   "log", #sigma
                   rep("lin", n_knots),  # beta spline coefficients
                   "lin", #S0 
                   "log", "log", "log","log", #E/Ia/Isu/Isd
                   "lin", "lin", "lin", "lin", #C/H/R/D
                   "lin" #trendO
                   )
  
  # do this for all parameters, even fixed ones
  coef_all <- data.frame(matrix(rep(allparvals,times = nrow(coef_est_df)) , nrow = nrow(coef_est_df), byrow = TRUE))
  colnames(coef_all) <- names(allparvals)
  
  # coef_all[,c(params_to_estimate,inivals_to_estimate)] = coef_est_df
  coef_all[,c(params_to_estimate)] = coef_est_df
  
  coef_natural_df <- transform_params(coef_all, param_trans)
  
  spline_names <- allparnames[grep("b", allparnames)[-1]]
  
  # also give some parameters new names to avoid confusion
  param_nat_names <- c("beta_s", 
                       "frac_trans_e", "frac_trans_a", "frac_trans_c", "frac_trans_h", 
                       "time_e", "time_a", "time_su", "time_sd", "time_c", "time_h", 
                       "max_diag_factor", "diag_rampup", "t_half_diag", 
                       "max_detect_frac", "detect_rampup", "t_half_detect",
                       "frac_asym", "frac_hosp", "frac_dead", 
                      "theta_cases", "theta_hosps", "theta_deaths", 
                      "sigma_dw", 
                      spline_names,
                      "S_0",
                      "E1_0", "Ia1_0", "Isu1_0", "Isd1_0",
                      "C1_0","H1_0","R_0","D_0",
                      "trendO_0")
  
  colnames(coef_natural_df) <- param_nat_names
  
  # combine this new dataframe with the ll_df as done above 
  # Also do some cleaning/renaming
  mif_result_natural_df <- ll_df %>%
    dplyr::rename("LogLik" = X1,
                  "LogLik_SE" = X2) %>%
    dplyr::mutate(MIF_ID = 1:n()) %>%
    dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
    bind_cols(coef_natural_df) %>%
    dplyr::arrange(-LogLik)
  
  is_fitted = rep("no",length(allparvals))
  is_fitted[which(names(allparvals) %in% c(params_to_estimate,inivals_to_estimate))] = "yes"
  
  mif_result_natural_df = data.frame(t(mif_result_natural_df)) #transpose
  mif_result_natural_df$is_fitted = c("no","no","no",is_fitted) #first 3 are MIF_ID, Loglik, SE
  
  res_list$partable_natural <- mif_result_natural_df %>% 
                               dplyr::select(is_fitted, everything())  
  
 
  # ---------------------------------------------------------
  # Likelihood slices for mif results
  # ---------------------------------------------------------
  if (1 == 2) #turn off this whole code block for now
  {
    # take best fit parameter values for each mif, run a particle filter to compute likelihood
    # scan over various parameters (while keeping others at MLE values)
    # produce likelihood slices
    # is too computationally intensive to do for all parameters, so we just do a few
    library(foreach)
    library(doParallel)
    library(tidyr)
    
    # turn on parallel running or not
    parallel_run <- TRUE
    num_cores <- parallel::detectCores() - 2  # alter as needed
    
    # Turn on parallel or not --------------------------------------------------
    if (parallel_run == TRUE) {
      # Set up parallel structure 
      n_cores <- num_cores
      cl <- makeCluster(num_cores) 
      registerDoParallel(cl)
    } else { #if not run in parallel, set this to 1
      n_cores <- 1
    }
    
    
    #do slice for each mif so we can compare
    for (i in 1:length(mifs))
    {
      
      print(sprintf('Start MIF number %d',i))
      p1 = coef(mifs[[i]])["log_beta_s"]
      p2 = coef(mifs[[i]])["frac_asym"]
      
      pslice <- sliceDesign(
        center=coef(mifs[[i]]),
        log_beta_s = rep(seq(from = 0.1*p1, to = 10*p1, length = 20),each=2) #the each value indicates how many replicates to do (since those are stochastic)  
        #frac_asym = rep(seq(from = 0.1*p2, to = 10*p2, length = 20) ,each=2)
      ) 
      
      slicefit <- foreach (theta=iter(pslice,"row"),
                           .combine=rbind,.inorder=FALSE, .packages = c("pomp")) %dopar% 
        {
          pf <- pomp_model %>% pfilter(params=theta,Np=200) 
          theta$loglik <- logLik(pf)
          return(theta)
        } 
    }
    stopCluster(cl)
    
    sliceplot <- slicefit %>% 
      tidyr::gather(variable,value,log_beta_s,frac_asym) %>%
      filter(variable==slice) %>%
      ggplot(aes(x=value,y=loglik,color=variable))+
      geom_point()+
      facet_grid(~variable,scales="free_x")+
      guides(color=FALSE)+
      labs(x="parameter value",color="")+
      theme_bw()
    
    res_list$sliceplot = sliceplot
    
    #plot(sliceplot)
    #filename = here('output/figures/LL-slice.png')
    #ggsave(filename,sliceplot)
  }
  
  return(res_list)
  
}

