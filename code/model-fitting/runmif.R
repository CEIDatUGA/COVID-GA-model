runmif <- function(parallel_info, mif_settings, pomp_model, par_var_list)
{

  # This uses the pomp::mif2() function to estimate parameters
  # of the stochastic SEIR model using maximization by iterated filtering.
  # The data are new daily case counts, new daily hospitalizations, and
  # new daily deaths. See the pomp_model object for details.
  
  #set file name to save results
  #either generic (which is overwritten)
  #or some unique name so it's not accidentally overwritten
  
  library(pomp)  # must be at least version 2.x
  library(doParallel)
  library(foreach)
  
  parallel_run <- parallel_info$parallel_run
  
  # Turn on parallel or not --------------------------------------------------
  if (parallel_run == TRUE) {
    # Set up parallel structure 
    n_cores <- parallel_info$num_cores
    cl <- makeCluster(n_cores) 
    registerDoParallel(cl)
  } else { #if not run in parallel, set this to 1
    n_cores <- 1
  }
  
  allparvals <- par_var_list$allparvals
  params_to_estimate = par_var_list$params_to_estimate
  inivals_to_estimate = par_var_list$inivals_to_estimate
  
  
  # Specify mif random walk intensity ---------------------------------
  # set up the rw.sd structure, i.e. perturbations for parameters needed for mif
  # assign perturbations following order of parameters above, 
  # hard-coded is not a good idea, but ok for now
  
  #this might need some more manual tweaking
  pert_par_vals <- rep(0.05, length(params_to_estimate))
  pert_ini_vals <- rep(0.1, length(inivals_to_estimate))
  # pert_par_vals <- c(rep(0.05,18), rep(0.1,3))
  # pert_ini_vals <- c(rep(0.2,12), rep(0.1,4))
  
  
  # make long string containing all parameter names and values
  # in a form required by rw.sd
  param_perts_string <- paste(params_to_estimate, 
                              '=',
                              pert_par_vals,
                              collapse = ', ')
  ini_perts_string <- paste0(inivals_to_estimate, 
                             ' = ivp(',
                             pert_ini_vals, 
                             ")", 
                             collapse = ', ')
  
  # this string is being fed into sw.rd inside mif 
  # below in a way suggested by Aaron
  if (inivals_to_estimate == "") {
    perts_string <- paste0("rw.sd(",param_perts_string,")")
  } else {
    perts_string <- paste0("rw.sd(",param_perts_string,", ",ini_perts_string,")")
  }
  
  
  # Define function that runs the whole mif --------------------------
  # function that runs the whole mif, either in parallel or not
  # does it in 2 parts. 
  
  run_mif <- function(pomp_model, num_mif_iterations, params, num_particles, 
                      c_frac, param_perts, verbose) 
  {
    out_mif <- pomp::mif2(pomp_model, # first part of mif run to converge
                          Nmif = num_mif_iterations[1], 
                          params = params, 
                          Np = num_particles[1], 
                          cooling.fraction.50 = c_frac[1], 
                          rw.sd = eval(parse(text=perts_string)),
                          cooling.type = "geometric",
                          verbose = verbose)
    
    out_mif <- out_mif %>% 
      pomp::continue( # 2nd part of mif run to 'polish off' things
        Nmif = num_mif_iterations[2],
        Np = num_particles[2], 
        cooling.fraction.50 = c_frac[2], 
        cooling.type = "geometric",
        verbose = verbose)
    
    return(out_mif)
  }
  
  
  # Compute different starting values for each run --------------------------
  # number of initial conditions to try, set to number of cores or multiple 
  # thereof for best performance
  n_ini_cond <- 1*n_cores 
  
  # set up matrix for starting values, each row is one set of initial conditions
  param_start <- matrix(0,nrow = n_ini_cond, ncol = length(params_to_estimate)) 
  
  # columns of matrix contain starting values for parameters to be estimated
  colnames(param_start) <- params_to_estimate 
  
  # fill matrix with starting values drawn from normal distribution
  for (i in 1:nrow(param_start)) {
    param_start[i,] = rnorm(length(params_to_estimate), 
                            allparvals[params_to_estimate], 
                            sd = 1) 
  } 
  
  # the mif2 routine needs numeric/starting conditions for 
  # both fixed and variable parameters
  fixed_params <- allparvals[!(names(allparvals) %in% params_to_estimate)] 
  

  mif_num_particles <- mif_settings$mif_num_particles
  mif_num_iterations <- mif_settings$mif_num_iterations 
  mif_cooling_fracs <- mif_settings$mif_cooling_fracs
  
  # For particle filter log likelihood estimation of MIF MLEs
  pf_num_particles <- mif_settings$pf_num_particles
  pf_reps <- mif_settings$pf_reps
  
  
  # Run MIF from different starting points ----------------------------------
  # Run MIF not-parallel
  
  tstart=Sys.time(); #capture current time, for benchmarking
  
  if (parallel_run == FALSE)
  {
    print(sprintf('Starting MIF2 + pfilter non-parallel at time %s',as.character(tstart)))
    out_mif = list()
    pf = list()
    for (i in 1:n_ini_cond) 
    {
      out_mif[[i]] <- run_mif(pomp_model,  
                              num_mif_iterations = mif_num_iterations, 
                              params = c(param_start[i,],fixed_params), 
                              num_particles = mif_num_particles, 
                              c_frac = mif_cooling_fracs, param_perts,
                              verbose = FALSE
                              ) -> test
  
      # Use particle filter to get the likelihood at the end of each MIF run
      pf[[i]] <- replicate(n = pf_reps, pfilter(out_mif[[i]], 
                                           Np = pf_num_particles, 
                                           max.fail = Inf))
      }
  } # end code section that does mif followed by pfilter for non-parallel setup
  
  # Run MIF parallel
  if (parallel_run == TRUE)
  {
    print(sprintf('Starting MIF2 + pfilter parallel at time %s',as.character(tstart)))
    out_mif <- foreach(i=1:n_ini_cond, .packages = c("pomp")) %dopar% 
      {
        run_mif(pomp_model, num_mif_iterations = mif_num_iterations, 
                params = c(param_start[i,],fixed_params), 
                num_particles = mif_num_particles, 
                c_frac = mif_cooling_fracs, param_perts,
                verbose = FALSE
                )
      } #end dopar/foreach to compute mif
  
    # Use particle filter to get the likelihood at the end of MIF run
    pf <- foreach(mf = out_mif, .packages = c("pomp")) %dopar% {
          replicate(n = pf_reps, pfilter(mf, Np = pf_num_particles, max.fail = Inf))
    }
    stopCluster(cl)
  } # end code section that does mif followed by pfilter for parallel setup
  
  tend=Sys.time(); #capture current time
  tdiff=as.numeric(difftime(tend,tstart,units='mins'))
  print(sprintf('Finished MIF2 + pfilter at time %s',as.character(tend)))
  print(sprintf('MIF took %f minutes using %d particles and %d MIF iterations',tdiff,mif_num_particles[1],sum(mif_num_iterations)))
  
  # Save output -------------------------------------------------------------
  # create a list of lists containing the mif runs 
  # and for each mif run, the subsequent pfilter runs 
  mifRets <- list(mif_runs = out_mif, pf_runs = pf)
  return(mifRets)
}
