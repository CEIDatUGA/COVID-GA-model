# run-particle-mcmc.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)
library(here)
library(doParallel)
library(foreach)


# Load the pomp object and mif results ------------------------------------

# load results produced by mif fitting ----------------------------------------------------
# this is a list of mif objects for each initial condition 
# followed by pfilter objects run a specified number of times after each mif is run
filename = here('output/mif-results.RDS')
mif_res_list <- readRDS(filename)
mifs = mif_res_list$mif_runs
pfs = mif_res_list$pf_runs


# Compute some results -------------------------------------------------------
# for each initial condition, take the pf runs and compute mean log likelihood
n_ini_cond = length(mifs)
ll = list()
for (i in 1:n_ini_cond) #do last part not in parallel
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


n_cores <- parallel::detectCores() - 2 # alter as needed
n_ini_cond <- n_cores 


# Extract MLEs as starting values -----------------------------------------

# Get the parameter set with the highest log likelihood and then
# pull those parameter estimates into a named vector
mle_inits <- pf_logliks %>%
  filter(LogLik == max(LogLik)) %>%  # get MLEs
  pull(MIF_ID)

mif_init <- mifs[[mle_inits]]



# Define the prior density ------------------------------------------------

prior_dens <- readRDS(here("output/prior-dens-object.RDS"))


# Run pMCMC ---------------------------------------------------------------

params_to_estimate <- names(coef(mif_init))
fixed_params <- c("S_0",
                  "C1_0",
                  "H1_0",
                  "R_0", "D_0")

rmones <- which(params_to_estimate %in% fixed_params)
params_to_estimate <- params_to_estimate[-rmones]
fixed_params <- coef(mif_init)[fixed_params]

# Set up chain starting points
param_start <- matrix(0, nrow = n_ini_cond, ncol = length(params_to_estimate)) 

# columns of matrix contain starting values for parameters to be estimated
colnames(param_start) <- params_to_estimate 

# fill matrix with starting values drawn from normal distribution
for (i in 1:nrow(param_start)) {
  param_start[i,] = rnorm(length(params_to_estimate), 
                          coef(mif_init)[params_to_estimate], 
                          sd = 1) 
} 


# Set noise level for parameter random walk for proposals
rw.sd <- rep(0.075, length(params_to_estimate))
names(rw.sd) <- params_to_estimate

# Forecast horizon, days
weeks_ahead <- 7  # go one further than 6 to make sure we get the dates
horizon <- 7 * weeks_ahead
newtimes <- c(time(mif_init), max(time(mif_init)) + seq_len(horizon))
newdata <- t(mif_init@data) %>%
  as.data.frame() %>%
  bind_rows(
    data.frame(cases = rep(NA, horizon),
               hosps = rep(NA, horizon),
               deaths = rep(NA, horizon)) 
  ) %>%
  t()

# Set covars table for forecasts
newcovars <- t(mif_init@covar@table) %>%
  as.data.frame() %>%
  bind_rows(
    data.frame(rel_beta_change = rep(tail(t(mif_init@covar@table), 1), times = horizon))
  ) %>%
  mutate(time = 1:n())

mif_init@data <- newdata
mif_init@times <- newtimes
mif_init@covar <- covariate_table(newcovars, times = "time", order = "constant")

pomp_for_mcmc <- pomp(
  mif_init,
  dprior = prior_dens,
  paramnames = params_to_estimate,
  cdir = getwd()  # just to fix a Windows error when compiling...
)

num_mcmc <- 1000

cl <- parallel::makeCluster(n_cores)
registerDoParallel(cl)

foreach(i = 1:n_cores, .combine = c, .packages = c("pomp")) %dopar% {
  pomp::pmcmc(
    pomp_for_mcmc,
    Nmcmc = num_mcmc,
    Np = 2000,
    proposal = pomp::mvn.diag.rw(rw.sd),
    verbose = FALSE
  )
} -> out_mcmc

stopCluster(cl)


# Save the output ---------------------------------------------------------

outfile <- here("output/pmcmc-output.RDS")
saveRDS(out_mcmc, outfile)




# Cache -------------------------------------------------------------------

# prior_dens <- Csnippet(
#   "
#   lik = dnorm(log_beta_s, -17.56266, 0.5, 1) +
#         dunif(trans_e, -5, 5, 1) +
#         dunif(trans_a, -5, 5, 1) +
#         dunif(trans_c, -15, 15, 1) +
#         dunif(beta_reduce, -5, 5, 1) +
#         dnorm(log_g_e, -0.2231436, 0.05, 1) +
#         dunif(log_g_a, -5, 5, 1) +
#         dunif(log_g_su, -5, 5, 1) +
#         dnorm(log_g_c, 5.764807, 1.157302, 1) +
#         dunif(log_g_h, -5, 5, 1) +
#         dnorm(log_diag_speedup, 0.6931472, 0.5, 1) +
#         dunif(detect_0, -5, 5, 1) +
#         dunif(detect_1, -5, 5, 1) +
#         dunif(log_theta_cases, -6, 6, 1) +
#         dunif(log_theta_hosps, -6, 6, 1) +
#         dunif(log_theta_deaths, -6, 6, 1) +
#         dnorm(E1_0, 40, 5, 1) +
#         dnorm(E2_0, 40, 5, 1) +
#         dnorm(E3_0, 40, 5, 1) +
#         dnorm(E4_0, 40, 5, 1) +
#         dnorm(Ia1_0, 22, 4, 1) +
#         dnorm(Ia2_0, 22, 4, 1) +
#         dnorm(Ia3_0, 22, 4, 1) +
#         dnorm(Ia4_0, 22, 4, 1) +
#         dnorm(Isu1_0, 90, 7, 1) +
#         dnorm(Isu2_0, 90, 7, 1) +
#         dnorm(Isu3_0, 90, 7, 1) +
#         dnorm(Isu4_0, 90, 7, 1) +
#         dnorm(Isd1_0, 14, 3, 1) +
#         dnorm(Isd2_0, 14, 3, 1) +
#         dnorm(Isd3_0, 14, 3, 1) +
#         dnorm(Isd4_0, 14, 3, 1);
#   
#   if (!give_log) lik = exp(lik);
#   "
# )

# test <- out
# beta <- test@traces[ , "log_beta_s"]
# theta <- exp(test@traces[, "log_theta_hosps"])
# hstates <- t(test@filter.traj["H_new",,]) %>%
#   as.data.frame() %>%
#   melt()
# hstates$hosps <- rnbinom(n = nrow(hstates), size = theta, mu = hstates$value)
# hstates$time = rep(newtimes, times = 20)
# hstates <- hstates %>%
#   mutate(period = ifelse(time < 43, "calibration", "forecast"))
# 
# pob <- readRDS("./output/pomp-model.RDS")
# dat <- t(pob@data) %>% as.data.frame()
# dat$time = 1:nrow(dat)
# 
# ggplot() +
#   geom_line(data = hstates, aes(x = time, y = value, group = variable, color = period)) +
#   geom_point(data = dat, aes(x = time, y = hosps), size = 1) +
#   geom_line(data = dat, aes(x = time, y = hosps), size = 0.3) +
#   xlab("Time since March 1") +
#   ylab("Number of new hospitalizations")
# 
# 
# theta <- exp(test@traces[1001:2000, "log_theta_cases"])
# hstates <- t(test@filter.traj["C_new",1001:2000,]) %>%
#   as.data.frame() %>%
#   melt()
# hstates$cases <- rnbinom(n = nrow(hstates), size = theta, mu = hstates$value)
# hstates  <- hstates %>%
#   group_by(variable) %>%
#   mutate(tot = cumsum(cases)) %>%
#   ungroup() %>% as.data.frame
# hstates$time = rep(c(0,newtimes), times = 1000)
# hstates <- hstates %>%
#   mutate(period = ifelse(time < 40, "calibration", "forecast"))
# 
# ggplot() +
#   geom_line(data = hstates, 
#             aes(x = time, y = tot, group = variable, color = period), alpha = 0.1) +
#   xlab("Time since March 1") +
#   ylab("Cumulative cases") 



# Cache -------------------------------------------------------------------

# 
# mcmcmat <- matrix(ncol = num_mcmc+1, nrow = length(out_mcmc))
# for(i in 1:length(out_mcmc)) {
#   mcmcmat[i, ] <- as.data.frame(out_mcmc[[i]]@traces)$log_beta_s
# }
# 
# matplot(t(exp(mcmcmat)*10600000), type = "l")



# saveRDS(object = out_mcmc, file = "../output/pomp-pmcmc-object.RDS")
# 
# chain <- as.data.frame(out_mcmc@traces)
# 
# par(mfrow = c(1,1))
# plot(exp(chain$log_beta_s)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[d]))
# plot(exp(chain$beta_u)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[u]))
# plot(exp(chain$beta_e)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[e]))
# plot(chain$beta_red_factor, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(xi))
# plot(chain$gamma_d, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(gamma[d]))
# plot(chain$gamma_u, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(gamma[u]))
# plot(chain$detect_frac_0, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = "detect_frac_0")
# plot(chain$theta, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(theta))
# 
