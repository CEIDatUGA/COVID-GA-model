# exploremifresults.R
# This function takes results produced by run-mif for exploration/plotting

exploremifresults <- function(pomp_res, par_var_list, n_knots)
{
  
  #  ---------------------------------------------------------
  #To-do ideas
  #Correlations of parameters across MIF iterations
  #bivariate histograms of likelihoods/posteriors
  #  ---------------------------------------------------------
  
  res_list=list() #this will contain all results produced here and returned 
  
  # Load libraries ----------------------------------------------------------
  #library(dplyr)
  #library(pomp)
  #library(purrr)
  #library(ggplot2)
  
  # pull out individual pieces from pomp_res super-list ----------------------------------------------------
  # this is a large list of objects 
  # mif_results are in sub-list mif_res 
  mif_res = pomp_res$mif_res
  
  # from mif_res object, extract mif run results for each run  
  mifs = sapply(mif_res, "[[", "out_mif")
  # followed by pfilter objects run a specified number of times after each mif is run
  
  
  pfs = sapply(mif_res, "[", "pf")
  
  
  pomp_model = pomp_res$pomp_model
  pomp_data = pomp_res$pomp_data   
  
  ############################################################################
  # take values for model parameters and initial conditions -----------------
  ############################################################################
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
  ll_df <- data.frame(matrix(unlist(ll), nrow=n_ini_cond, byrow=T)) %>%
    dplyr::rename("LogLik" = X1,
                  "LogLik_SE" = X2) %>%
    dplyr::mutate(MIF_ID = 1:n()) %>%
    dplyr::select(MIF_ID, LogLik, LogLik_SE)
  
  # extract best fit paramter values for each mif run
  all_par_df <- data.frame(matrix(unlist(sapply(mifs, coef)), 
                                  nrow = length(mifs), 
                                  byrow = T))
  colnames(all_par_df) <- names(coef(mifs[[1]]))  
  
  est_par_df = all_par_df %>% dplyr::select( tidyselect::all_of(params_to_estimate))
  
  
  # combine the ll_df and parameter  data frames. 
  all_ll_par_df <- ll_df  %>% bind_cols(all_par_df) %>%  dplyr::arrange(-LogLik)
  est_ll_par_df <- ll_df  %>% bind_cols(est_par_df) %>%  dplyr::arrange(-LogLik)
  
  res_list$est_partable = est_ll_par_df
  res_list$all_partable = all_ll_par_df
  
  
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
                   "logis", "log", "log", "log",  #detect
                   "logis", "logis", "logis", #frac
                   "log", "log", "log", #theta
                   "log", #sigma
                   rep("lin", times = n_knots),
                   "lin", #S0 
                   "log", "log", "log","log", #E/Ia/Isu/Isd
                   "lin", "lin", "lin", "lin", #C/H/R/D
                   "lin"
  )
  
  # do this for all parameters, even fixed ones
  coef_all <- data.frame(matrix(rep(allparvals,times = nrow(all_par_df)) , nrow = nrow(all_par_df), byrow = TRUE))
  colnames(coef_all) <- names(allparvals)
  
  coef_all[,c(params_to_estimate)] = est_par_df
  
  natural_par_df <- transform_params(coef_all, param_trans)
  
  # also give some parameters new names to avoid confusion
  param_nat_names <- c("beta_s", 
                       "frac_trans_e", "frac_trans_a", "frac_trans_c", "frac_trans_h", 
                       "time_e", "time_a", "time_su", "time_sd", "time_c", "time_h", 
                       "max_diag_factor", "diag_rampup", "t_half_diag",
                       "max_detect_frac", "detect_rampup", "t_half_detect", "base_detect_frac",
                       "frac_asym", "frac_hosp", "frac_dead", 
                       "theta_cases", "theta_hosps", "theta_deaths", 
                       "sigma_dw", 
                       paste0("b",1:n_knots),
                       "S_0",
                       "E1_0", "Ia1_0", "Isu1_0", "Isd1_0",
                       "C1_0","H1_0","R_0","D_0", "trend_start")
  
  colnames(natural_par_df) <- param_nat_names
  
  # combine this new dataframe with the ll_df as done above 
  # Also do some cleaning/renaming
  mif_result_natural_df <- ll_df  %>% bind_cols(natural_par_df) %>%  dplyr::arrange(-LogLik)
  
  is_fitted = rep("no",length(allparvals))
  is_fitted[which(names(allparvals) %in% c(params_to_estimate,inivals_to_estimate))] = "yes"
  
  mif_result_natural_df = data.frame(t(mif_result_natural_df)) #transpose
  mif_result_natural_df$is_fitted = c("no","no","no",is_fitted) #first 3 are MIF_ID, Loglik, SE
  
  res_list$partable_natural <- mif_result_natural_df %>% 
    dplyr::select(is_fitted, everything())  
  
  return(res_list)
  
}


