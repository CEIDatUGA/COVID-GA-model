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


# Define parameter and variable names -------------------------------------     
# define parameters to be estimated
# is passed to setparsvars function. 
# If set to "all", all params are estimated

# Parameters
est_these_pars = c("log_beta_s", 
                   "frac_dead", "log_half_detect", "log_half_diag",
                   "max_detect_par",
                   "log_sigma_dw",
                   "log_theta_cases", "log_theta_deaths")

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
                            tint = 12)  # tint = March 12 assuming March 1 start


# Set priors --------------------------------------------------------------     
# needs results from setparsvars 
source(here("code/model-setup/setpriors.R"))
prior_dens <- setpriors(par_var_list)  # for ABC and pMCMC routines


# Run data cleaning script. Return data ready for pomp --------------------

source(here("code/data-processing/loadcleanCTdata.R"))
source(here("code/data-processing/loadcleanGDPHdata.R"))

if (datasource == "COV") {
  pomp_data <- loadcleanCTdata(use_these_locations = location, start_date = "2020-03-01")
}
if (datasource == "GAD") {  
  pomp_data <- loadcleanGDPHdata(start_date = "2020-03-01")
}

# Apply 7-day moving average to the data
ma <- function(x) {
  window <- 7
  n <- c(seq.int(window), rep(window, length(x)-window))
  xm <- ceiling(data.table::frollmean(x, n, adaptive=TRUE, na.rm = T))
  xm[is.nan(xm)] <- NA
  return(xm)
}

pomp_data <- pomp_data %>%
  mutate(cases = ma(cases),
         # hosps = ma(hosps),
         deaths = ma(deaths))



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

# Make sure that the covariate and data times match
stopifnot(nrow(covar_table) == nrow(pomp_data))


# Make a pomp model -------------------------------------------------------
# use data, covariate and parameter information to make a 
# pomp model that's ready for fitting
source(here("code/model-setup/makepompmodel.R"))
# covar_table2 <- covar_table
# covar_table2$rel_beta_change[40:80] = 0.4
pomp_model <- makepompmodel(par_var_list = par_var_list, 
                            pomp_data = pomp_data, 
                            covar_table = covar_table2)

# params <- par_var_list$allparvals
# params["log_half_detect"] <- log(12)
# params["log_half_diag"] <- log(12)
# # params["log_max_diag"] <- log(1)
# 1/(1+exp(1))
# params["max_detect_par"] <- 0.5
# params["frac_dead"] <- 0.25
# params["log_g_h"] = log(4/6)
# params["log_g_c"] = log(4/3)
# sim <- simulate(pomp_model, nsim = 1, params = params, format = "data.frame")
# tmax <- nrow(sim)
# par(mfrow=c(1,2))
# plot(sim$C_new[1:tmax], type = "l")
# lines(pomp_data$cases[1:tmax], lty = 2)
# plot(sim$D_new[1:tmax], type = "l")
# lines(pomp_data$deaths[1:tmax], lty = 2)

# pf <- pfilter(pomp_model,Np=1000,params=params)
# logLik(pf)
# 
# t <- 1:80
# diag_speedup <- 1 + exp(params["log_max_diag"]) * exp(params["log_diag_inc_rate"])^t /  ( exp(params["log_diag_inc_rate"])^exp(params["log_half_diag"]) +   exp(params["log_diag_inc_rate"])^t    )
# g_sd = diag_speedup*exp(params["log_g_sd"]) 
# g_c = exp(log_g_c)/diag_speedup; 

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
mif_explore <- exploremifresults(mif_res = mif_res)

#add results from mif exploration to mif_res object
mif_res$traceplot <- mif_explore$traceplot
mif_res$partable <- mif_explore$partable
mif_res$partable_natural <- mif_explore$partable_natural


# Save complete MIF object and all data used to create it -----------------
# saved in a permanent file with time-stamp
filename_mif <- here('output', paste0(filename_label, '_mif.rds'))
saveRDS(object = mif_res, file = filename_mif)


# Simulate the model to predict -----------------------------------------------------

# Source the function to simulate trajectories and scenarios
source(here("code/forward-simulations/simulate_trajectories.R"))

# Source the script run the scenarios -- saves a file this time
source(here("code/forward-simulations/run-scenarios.R"))

# source(here("code/forecasting-code/format-forecasts.R"))


# Make the plots for the website ------------------------------------------

# if(datasource == "COV") {
#   fig_outpath <- here("output/figures/covidtracker-figures/")
# }
# if(datasource == "GAD") {
#   fig_outpath <- here("output/figures/gadph-figures/")
# }

# source(here("code/plotting/plot-scenarios.R"))
# 
# # And add the mif trace
# ggsave(filename = paste0(fig_outpath, "/mif-trace.png"), 
#        plot = mif_res$traceplot)




