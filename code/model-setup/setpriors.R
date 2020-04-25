setpriors <- function(par_var_list)
{

  # make-priors.R
  library(pomp)
  
  #values for priors are explained in a separate spreadsheet:
  #https://docs.google.com/spreadsheets/d/10K_3bPck0GOCAfuUQtyZ-KEmwf6FQE3CcpSe36HtL4c/edit#gid=478517635
  
  # code below implements them so they can be used by pomp
  # mean/central values for parameters, as specified by spreadsheet
  # for meaning of parameters, see spreadhseet
  
  x <- par_var_list
  
  param_sds <- 3
  prior_par_list = list()
  prior_ini_list = list()
  ini_sds <- 1
  
  #makes priors of form "dnorm(par,mu,param_sd,1)
  
  for (nn in 1:length(x$params_to_estimate))
  {
    prior_par_list[[nn]] = paste0("dnorm(", x$params_to_estimate[nn],", ",as.numeric(x$allparvals[x$params_to_estimate[nn]]),", ",param_sds, ", 1)")
  }
  
  for (nn in 1:length(x$inivals_to_estimate)) #need different sds, so extra loop
  {
    prior_ini_list[[nn]] = paste0("dnorm(", x$inivals_to_estimate[nn],", ",x$allparvals[x$inivals_to_estimate[nn]],", ",ini_sds, ", 1)")
  }
  
  s1 = paste0(prior_par_list, collapse = " + ")
  s2 = paste0(prior_ini_list, collapse = " + ")
  prior_dens_text = paste0("lik = ",s1," + ",s2, " ; \n " ,
                           "if (!give_log) lik = exp(lik);" )
  
  prior_dens <- pomp::Csnippet(
    prior_dens_text
  )
  #
  return(prior_dens)
}

