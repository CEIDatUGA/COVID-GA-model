# MASTER-RUN-ROUTINE.R
# This script is designed to run the entire workflow for estimating
# parameters for the stochastic COVID-19 SEIR model and for producing
# estimates of observed states and forecasts of states. Many of the scripts
# rely on parallel processing, the settings for which are defined in this
# master script.
#
# NOTE: The analyses performed here require the 
#       R package `pomp` v2.x or greater.


# Start with a clean workspace to avoid downstream errors -----------------
# rm(list = ls(all.names = TRUE))

# args <- commandArgs(trailingOnly = F)
# myargument <- args[length(args)]
# myargument <- as.numeric(sub("-","",myargument))
myargument <- 1

# Necessary libraries ------------------------------------------------------
# We only libraries needed for this script, others are loaded in each code
# so we can run various scripts independently 
# these libraries are needed by various scripts
library(here)
library(pomp)

# Set state, data source and a time-stamp variable -------------------------     

# Location
location <- c("Georgia")  # US state to model

# Data source: one of CovidTracker (COV) or GA DPH (GAD)
if(myargument == 1) {
  datasource <- "COV"
}
if(myargument == 2) {
  datasource <- "GAD"
}

# Time stamp for results and output
tm <- .POSIXct(Sys.time(), "US/Eastern")  # time stamp with date, hours, minutes
stamp <- paste(lubridate::date(tm),
               stringr::str_pad(as.character(lubridate::hour(tm)), 
                                width = 2, side = "left", pad = "0"),
               stringr::str_pad(as.character(lubridate::minute(tm)), 
                                width = 2, side = "left", pad = "0"),
               sep='-')

# This will be appended to each saved file 
filename_label <- paste(location,datasource,stamp,sep="_") 


# Run data cleaning script. Return data ready for pomp --------------------

source(here("code/data-processing/loadcleanCTdata.R"))
source(here("code/data-processing/loadcleanGDPHdata.R"))

if (datasource == "COV") {
  pomp_data <- loadcleanCTdata(use_these_locations = location, start_date = "2020-03-01")
}
if (datasource == "GAD") {  
  pomp_data <- loadcleanGDPHdata(start_date = "2020-03-01")
}


# Define parameter and variable names -------------------------------------     
# define parameters to be estimated
# is passed to setparsvars function. 
# If set to "all", all params are estimated

# Parameters
est_these_pars = c("log_beta_s", 
                   "frac_hosp", "frac_dead", 
                   "max_detect_par", 
                   "log_sigma_dw",
                   "log_theta_cases", "log_theta_hosps", "log_theta_deaths")
n_knots <- round(nrow(pomp_data) / 7)
knot_coefs <-  paste0("b", 1:n_knots)
est_these_pars <- c(est_these_pars, knot_coefs)

# Initial conditions
# est_these_inivals = c("E1_0", "Ia1_0", "Isu1_0", "Isd1_0")
est_these_inivals = ""  # no initial conditions


# source function which assigns values to variables and initial conditions
# specifies parameters that are being fitted
source(here("code/model-setup/setparsvars.R"))

# run function that sets variables and parameters 
# functions doesn't return anything, results are written to file
par_var_list <- setparsvars(est_these_pars = est_these_pars, 
                            est_these_inivals = est_these_inivals, 
                            tint = 12,  # tint = March 12 assuming March 1 start
                            n_knots = n_knots)  


# Set priors --------------------------------------------------------------     
# needs results from setparsvars 
# source(here("code/model-setup/setpriors.R"))
# prior_dens <- setpriors(par_var_list)  # for ABC and pMCMC routines




# Read in the movement data covariate table -------------------------------

covar_file <- tail(
  list.files(path = here("data/"), pattern = "rel-beta-change"),
  1)
covar_table <- readRDS(here(paste0("data/", covar_file)))
covar_table <- covar_table %>%
  dplyr::select(-time) %>%
  right_join(pomp_data %>%
              dplyr::select(Date, cases), by = "Date") %>%
  tidyr::fill(rel_beta_change) %>%  # fills in trailing NAs w/ last data point
  dplyr::select(-cases, -Date) %>%
  mutate(time = 1:n()) %>%
  # truncate the upper bound at 1, just rounding, really
  mutate(rel_beta_change = ifelse(rel_beta_change > 1, 1, rel_beta_change))


covar = covariate_table(
  t = pomp_data$time,
  seas = bspline.basis(
    x=t,
    nbasis=n_knots,
    degree=3
  ),
  rel_beta_change = as.matrix(covar_table$rel_beta_change),
  times="t",
  order = "constant"
)


# Make sure that the covariate and data times match
# stopifnot(nrow(covar_table) == nrow(pomp_data))


# Make a pomp model -------------------------------------------------------
# use data, covariate and parameter information to make a 
# pomp model that's ready for fitting
source(here("code/model-setup/makepompmodel.R"))
pomp_model <- makepompmodel(par_var_list = par_var_list, 
                            pomp_data = pomp_data, 
                            covar_table = covar,
                            n_knots = n_knots)

# Simulate from the model as a test
# simparams <- par_var_list$allparvals
# betas <- rnorm(n_knots, 0, 10)
# betanames <- paste0("b", 1:n_knots)
# simparams[betanames] <- betas
# sims <- simulate(pomp_model, nsim = 1,
#                  params = simparams, format="data.frame")
# plot(sims$cases)

# Run the mif fitting routine ---------------------------------------------
# turn on parallel running or not
parallel_info = list()
parallel_info$parallel_run <- TRUE
# parallel_info$num_cores <- parallel::detectCores() - 2  # alter as needed
parallel_info$num_cores <- 30  # on HPC

# specify settings for mif2 procedure
# two rounds of MIF
# these 2 rounds are currently hard-coded into runmif
mif_settings = list()
mif_settings$mif_num_particles  <- c(2000, 2000)
mif_settings$mif_num_iterations <- c(100, 100)
mif_settings$mif_cooling_fracs <- c(0.9, 0.7)
mif_settings$pf_num_particles <- 5000
mif_settings$pf_reps <- 10

# source the mif function
source(here("code/model-fitting/runmif.R"))

# supply all info to mif and run it 
# output is list containing an object of mif runs and
# an object of pfilter runs for each mif
mif_res <- runmif(parallel_info = parallel_info, 
                   mif_settings = mif_settings, 
                   pomp_model = pomp_model, 
                   par_var_list = par_var_list)

#add all parts used for mif result to this list
#this now includes the complete information for a given mif run
#not saving the prior object since it's not used by mif
mif_res$pomp_model <- pomp_model 
mif_res$pomp_data <- pomp_data 
mif_res$par_var_list <- par_var_list 
mif_res$location <- location 
mif_res$covar_table <- covar_table 
mif_res$datasource <- datasource
mif_res$filename_label <- filename_label


# Post process mif results ------------------------------------------------     
# currently returns a trace plot figure (as ggplot object)
# and 2 parameter tables. optional if turned on a likelihood slice plot
source(here("code/result-exploration/exploremifresults.R"))
mif_explore <- exploremifresults(mif_res = mif_res, n_knots = n_knots)

#add results from mif exploration to mif_res object
mif_res$traceplot <- mif_explore$traceplot
mif_res$partable <- mif_explore$partable
mif_res$partable_natural <- mif_explore$partable_natural


# Save complete MIF object and all data used to create it -----------------
# saved in a permanent file with time-stamp
filename_mif <- here('output', paste0(filename_label, '_mif.rds'))
saveRDS(object = mif_res, file = filename_mif)


# Simulate mode to test
mifs <- mif_res$mif_runs
pfs <- mif_res$pf_runs
pomp_model <- mif_res$pomp_model

n_ini_cond = length(pfs)
ll = list()
for (i in 1:n_ini_cond) {
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

sims <- matrix(NA, nrow = nrow(pomp_data), ncol = nrow(all_mles))
for(i in 1:nrow(all_mles)) {
  simsraw <- simulate(mif_res$pomp_model, nsim = 100, format = "data.frame", params = all_mles[i,])
  simsout <- simsraw %>%
    group_by(time) %>%
    summarise(cases = mean(cases))
  sims[,i] <- simsout$cases
}
matplot(sims, type = "l")
lines(rowMeans(sims), lwd = 5)
# plot(sims$cases, type  ='l', ylim = c(0,2000))
points(pomp_data$cases, pch = 19)

# Simulate the model to predict -----------------------------------------------------

# # Source the function to simulate trajectories and scenarios
# source(here("code/forward-simulations/simulate_trajectories.R"))
# 
# # Source the script run the scenarios -- saves a file this time
# source(here("code/forward-simulations/run-scenarios.R"))


# Make the plots for the website ------------------------------------------

# if(datasource == "COV") {
#   fig_outpath <- here("output/figures/covidtracker-figures/")
# }
# if(datasource == "GAD") {
#   fig_outpath <- here("output/figures/gadph-figures/")
# }
# 
# source(here("code/plotting/plot-scenarios.R"))
# 
# # And add the mif trace
# ggsave(filename = paste0(fig_outpath, "/mif-trace.png"), 
#        plot = mif_res$traceplot)




