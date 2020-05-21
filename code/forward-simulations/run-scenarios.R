# run-scenarios.R

# Load libraries ----------------------------------------------------------

library(pomp)
library(tidyverse)
library(here)
source(here("code/forward-simulations/simulate_trajectories.R"))

# Load pomp model and MLEs ------------------------------------------------

all_mif <- readRDS(filename_mif)
pfs <- all_mif$pf_runs
mifs <- all_mif$mif_runs
pomp_model <- all_mif$pomp_model

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
  filter(!is.nan(LogLik)) %>%
  filter(LogLik >= (max(LogLik)-2)) %>%
  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)

# Make sure there are some decent MLEs, i.e., not -inf
stopifnot(nrow(all_mles) > 0)

obs_sim <- tibble()
for(i in 1:nrow(all_mles)) {
  sim <- simulate(pomp_model,
                      params = all_mles[i, ],
                      nsim = 100,
                      format="data.frame") %>%
    mutate(mle_id = i)
  obs_sim <- bind_rows(obs_sim, sim)
}

obs_sim2 <- simulate(pomp_model,
                    params = all_mles[1, ],
                    nsim = 1,
                    format="data.frame",
                    include.data = TRUE)
obs_sim2 <- obs_sim2 %>%
  filter(.id == "data") %>% 
  mutate(mle_id = 999)

obs_sim <- bind_rows(obs_sim, obs_sim2)



# Run simulations ---------------------------------------------------------
weeks_ahead <- 6
num_sims <- 100

out_sims <- tibble()  # empty storage object
covar_scens <- tibble()  # empty storage object
for(i in 1:nrow(all_mles)){
  mles <- all_mles[i, ]
  obs <- obs_sim %>% 
    filter(mle_id %in% c(i, 999))
  
  sim_sql <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "status_quo", param_vals = mles,
                                  forecast_horizon_wks = weeks_ahead, 
                                  nsims = num_sims, obs_sim = obs) 
  sim_sq <- sim_sql$sims_ret %>%
    mutate(SimType = "status_quo")
  
  sim_nal <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "no_intervention", 
                                  covar_no_action = 1,
                                  param_vals = mles,
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims, obs_sim = obs)
  sim_na <- sim_nal$sims_ret %>%
    mutate(SimType = "no_intervention") %>%
    mutate(.id = as.character(.id))  # added to match the non-counterfactual returns
  
  sim_minsdl <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                     covar_action = "lowest_sd", 
                                     param_vals = mles,
                                     forecast_horizon_wks = weeks_ahead,
                                     nsims = num_sims, obs_sim = obs)
  sim_minsd <- sim_minsdl$sims_ret %>%
    mutate(SimType = "lowest_sd") %>%
    mutate(.id = as.character(.id))  # added to match the non-counterfactual returns
  
  sim_msdl <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "more_sd",
                                  param_vals = mles, 
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims, obs_sim = obs) 
  sim_msd <- sim_msdl$sims_ret %>%
    mutate(SimType = "linear_increase_sd")
  
  sim_lsdl <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                  covar_action = "less_sd",
                                  param_vals = mles, 
                                  forecast_horizon_wks = weeks_ahead,
                                  nsims = num_sims, obs_sim = obs)
  sim_lsd <- sim_lsdl$sims_ret %>%
    mutate(SimType = "linear_decrease_sd")
  
  sim_norl <- simulate_trajectories(pomp_model, start_date = "2020-03-01",
                                   covar_action = "normal",
                                   param_vals = mles, 
                                   forecast_horizon_wks = weeks_ahead,
                                   nsims = num_sims, obs_sim = obs)
  sim_nor <- sim_norl$sims_ret %>%
    mutate(SimType = "return_normal")
  
  all_sims <- bind_rows(sim_sq, sim_na, sim_minsd, 
                        sim_msd, sim_lsd, sim_nor) %>%
    mutate(mle_id = i,
           rep_id =  paste(.id, mle_id, sep = "-"))
  out_sims <- bind_rows(out_sims, all_sims)
  
  # Collate the covariate scenarios
  cov_sq <- sim_sql$covars %>%
    mutate(SimType = "status_quo")
  cov_na <- sim_nal$covars %>%
    mutate(SimType = "no_intervention")
  cov_minsd <- sim_minsdl$covars %>%
    mutate(SimType = "lowest_sd") 
  cov_msd <- sim_msdl$covars %>%
    mutate(SimType = "linear_increase_sd")
  cov_lsd <- sim_lsdl$covars %>%
    mutate(SimType = "linear_decrease_sd")
  cov_nor <- sim_norl$covars %>%
    mutate(SimType = "return_normal")
  all_covars <- bind_rows(cov_sq, cov_na, cov_minsd, cov_msd, cov_lsd, cov_nor)
  covar_scens <- bind_rows(covar_scens, all_covars)
}

# Save the simulations
fname <- here('output', paste0(filename_label, '_simulation-scenarios.rds'))
saveRDS(object = out_sims, file = fname)

# Save the covariates
fname2 <- here('output', paste0(filename_label, '_simulation-covariates.rds'))
saveRDS(object = covar_scens, file = fname2)


