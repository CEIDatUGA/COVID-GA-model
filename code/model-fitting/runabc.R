# run-abc.R
# This script is designed to start where mif leaves off. Several
# ABC MCMC chains are initiated from the parameter estimates from
# MIF runs. The ABC MCMC is run for several hundred thousand iterations
# to achieve stable posterior distributions of parameters that are
# informed by prior distributions. The ABC MCMC results are then passed
# to the pMCMC script for final inference and forecasting.



# Load libraries ----------------------------------------------------------
library(tidyverse)
library(pomp)
library(here)
library(doParallel)
library(foreach)


# Define summary statistic (probes) functions -----------------------------

get_stat_times <- function(obs_cases) {
  x <- obs_cases
  ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}
  stat_times <- ma(diff(log(x)), n = 5)
  d0 <- min(which(stat_times > 0.1))
  suppressWarnings(
    d1 <- d0 + min(which(stat_times[d0:length(stat_times)] < -0.01))
  )
  return(c(d0 = d0, d1 = d1))
}

ds <- get_stat_times(pomp_data$cases)
d0 <- ds["d0"]
d1 <- ds["d1"]
if(is.infinite(d1)) d1 <- nrow(pomp_data) - 1
d2 <- nrow(pomp_data)

# pomp won't allow variables in the probe functions, so we have to make
# these time-dependent ones on the fly here.
# Cases data
max1 <- eval(parse(text = paste0("function(x){ max(log(x[1, 1:(", d0, "-1)] + 10))  }")))
max2 <- eval(parse(text = paste0("function(x){ max(log(x[1,", d0, ":", d1, "] + 10))  }")))
max3 <- eval(parse(text = paste0("function(x){ max(log(x[1,(", d1, "+1):", d2, "] + 10))  }")))
maxall <- function(x){ max(log(x[1,])) }
maxday <- function(x){ which.max(x[1,]) }
cumi1 <- eval(parse(text = paste0("function(x){ sum(log(x[1, 1:(", d0, "-1)] + 10))  }")))
cumi2 <- eval(parse(text = paste0("function(x){ sum(log(x[1, ", d0, ":", d1, "] + 10))  }")))
cumi3 <- eval(parse(text = paste0("function(x){ sum(log(x[1, (", d1, "+1):", d2, "] + 10))  }")))
exp1 <- eval(parse(text = paste0("function(x) { max(x[1, ]) / which.max(x[1, ]) -", d0, "}")))
regcoef <- function(x) { as.numeric(coef(lm(x[1,] ~ seq_along(x[1,])))[2]) }

# Hosps data
hmax1 <- eval(parse(text = paste0("function(x){ max(log(x[2, 1:(", d0, "-1)] + 10))  }")))
hmax2 <- eval(parse(text = paste0("function(x){ max(log(x[2,", d0, ":", d1, "] + 10))  }")))
hmax3 <- eval(parse(text = paste0("function(x){ max(log(x[2,(", d1, "+1):", d2, "] + 10))  }")))
hmaxall <- function(x){ max(log(x[2,])) }
hmaxday <- function(x){ which.max(x[2,]) }
hcumi1 <- eval(parse(text = paste0("function(x){ sum(log(x[2, 1:(", d0, "-1)] + 10))  }")))
hcumi2 <- eval(parse(text = paste0("function(x){ sum(log(x[2, ", d0, ":", d1, "] + 10))  }")))
hcumi3 <- eval(parse(text = paste0("function(x){ sum(log(x[2, (", d1, "+1):", d2, "] + 10))  }")))
hexp1 <- eval(parse(text = paste0("function(x) { max(x[2, ]) / which.max(x[1, ]) -", d0, "}")))
hregcoef <- function(x) { as.numeric(coef(lm(x[2,] ~ seq_along(x[2,])))[2]) }

# Deaths data
dmax1 <- eval(parse(text = paste0("function(x){ max(log(x[3, 1:(", d0, "-1)] + 10))  }")))
dmax2 <- eval(parse(text = paste0("function(x){ max(log(x[3,", d0, ":", d1, "] + 10))  }")))
dmax3 <- eval(parse(text = paste0("function(x){ max(log(x[3,(", d1, "+1):", d2, "] + 10))  }")))
dmaxall <- function(x){ max(log(x[3,])) }
dmaxday <- function(x){ which.max(x[3,]) }
dcumi1 <- eval(parse(text = paste0("function(x){ sum(log(x[3, 1:(", d0, "-1)] + 10))  }")))
dcumi2 <- eval(parse(text = paste0("function(x){ sum(log(x[3, ", d0, ":", d1, "] + 10))  }")))
dcumi3 <- eval(parse(text = paste0("function(x){ sum(log(x[3, (", d1, "+1):", d2, "] + 10))  }")))
dexp1 <- eval(parse(text = paste0("function(x) { max(x[3, ]) / which.max(x[1, ]) -", d0, "}")))
dregcoef <- function(x) { as.numeric(coef(lm(x[3,] ~ seq_along(x[3,])))[2]) }




# Set ABC algorithm settings ----------------------------------------------

# Set noise level for parameter random walk for proposals
rw.sd <- rep(0.175, length(par_var_list$allparvals))
names(rw.sd) <- names(par_var_list$allparvals)

# Define the probe list
plist <- list(
  max1, max2, max3, maxall, cumi1, cumi2, cumi3, exp1, regcoef,
  hmax1, hmax2, hmax3, hmaxall, hcumi1, hcumi2, hcumi3, hexp1, hregcoef,
  dmax1, dmax2, dmax3, dmaxall, dcumi1, dcumi2, dcumi3, dexp1, dregcoef
)

# Make a new pomp object for ABC
abc_pomp_object <- pomp(pomp_model,
                        dprior = prior_dens,
                        paramnames = par_var_list$parnames,
                        cdir = getwd())  # cdir to avoid weird windows error

# The statistics don't like NA, switch to 0 for now
abc_pomp_object@data[which(is.na(abc_pomp_object@data))] <- 0 



# Test the probes and get scale for each ----------------------------------

psim <- probe(abc_pomp_object,
              params = coef(mifs[[1]]), 
              probes = plist,
              nsim = 1000)

scale_dat <- apply(psim@simvals, 2, sd)


# Run the ABC-MCMC with MIF starting values -------------------------------

# For ABC-MCMC
abc_num_mcmc <- 200000
# abc_num_burn <- abc_num_mcmc/2
# abc_num_thin <- (abc_num_mcmc - abc_num_burn) * 0.0004
# abc_num_thin <- 1

start_coefs <- lls %>%
  dplyr::select(-MIF_ID, -LogLik, -LogLik_SE)

num_cores <- length(nrow(lls))  # alter as needed
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)

foreach(i = 1:nrow(lls), .combine = c, .packages = c("pomp"),
        .export = c("prior_dens", "params_to_estimate", "rw.sd",
                    "abc_pomp_object", "abc_num_mcmc", "plist", "scale_dat",
                    "start_coefs")) %dopar% {
                      pomp::abc(
                        pomp::pomp(
                          abc_pomp_object,
                          params = start_coefs[i,],
                        ),
                        Nabc = abc_num_mcmc,
                        epsilon = 70,
                        scale = scale_dat,
                        proposal = mvn.diag.rw(rw.sd),
                        probes = plist,
                        verbose = FALSE
                      ) 
                    } -> out_abc

stopCluster(cl)

saveRDS(out_abc, file = here("output/abc-results.RDS"))




# 
# t1 <- tail(out@traces[,1], 10000)
# ttt <- t1[seq(1, length(t1), 0)]
# plot(ttt, type = "l")
# plot(out@traces[,8], type = "l")
# abline(h = -17.0927194398423, col = "red")
# out_abc <- list("1" = out)
# Summarize parameters ----------------------------------------------------

# all_abc <- tibble()
# for(i in 1:length(out_abc)) {
#   tmp <- out_abc[[i]]
#   param_mat <- tail(tmp@traces, abc_num_mcmc - abc_num_burn) %>%
#     as.data.frame() %>%
#     mutate(chain = i,
#            iter = 1:n())
#   param_out <- param_mat[seq(1, nrow(param_mat), abc_num_thin), ]
#   all_abc <- bind_rows(all_abc, param_out)
# }
# 
# abc_summaries <- all_abc %>%
#   gather(key = "Parameter", value = "Value", -chain) %>%
#   group_by(Parameter, chain) %>%
#   summarise(lower = quantile(Value, 0.025),
#             median = quantile(Value, 0.5),
#             upper = quantile(Value, 0.975),
#             mean = mean(Value),
#             sd = sd(Value))
# 
# abc_params <-  abc_summaries %>% filter(chain == 1) %>% dplyr::select(Parameter, mean) %>%
#   filter(Parameter != "iter")
# mif_params <- t(start_coefs[1, ]) %>% as.data.frame()
# mif_params$Parameter <- row.names(mif_params)
# mif_params <- mif_params %>% left_join(abc_params) %>% mutate(diff = `1` - mean)

# abc_summaries %>% filter(chain == 1) %>% dplyr::select(Parameter, mean) %>%
#   filter(Parameter != "iter") %>% deframe() -> allparvals


# Save the results --------------------------------------------------------

# abc_results <- list(abc_chains = all_abc, abc_summaries = abc_summaries)
# saveRDS(object = abc_results, file = here("output/abc-results"))


all_abc %>% 
  dplyr::select(log_beta_s, chain, iter) %>%
  gather(key = "param", value = "value", -chain, -iter) %>%
  ggplot(aes(x = iter, y = value, color = as.factor(chain), group = chain)) +
  geom_line()
  






# Cache -------------------------------------------------------------------


# out_abc <- abc(
#   pomp(
#     abc_pomp_object,
#     params = coef(mifs$mif_objects[[1]]),
#     dprior = prior_dens,
#     paramnames = params_to_estimate
#   ),
#   Nabc = 100000,
#   epsilon = 5,
#   scale = scale.dat,
#   proposal = mvn.diag.rw(rw.sd),
#   probes = plist,
#   verbose = TRUE
# )
# 
# plot(out_abc)


# saveRDS(object = out_mcmc, file = "../output/pomp-pmcmc-object.RDS")

# chain <- as.data.frame(out_abc@traces)[100000:200000,]
# chain <- chain[seq(1, nrow(chain), by = 10),]

# par(mfrow = c(4,2))
# plot(exp(chain$beta_d)*10600000, type = "l", bty = "n",
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
# chain <- as.data.frame(out_abc@traces)
# par(mfrow = c(4, 2))
# plot(density(exp(chain$beta_d)*10600000, adjust = 1), bty = "n",
#      ylab = "Density", xlab = expression(beta[d]), main = "")
# lines(density(exp(rnorm(100000, log(2e-7), 0.8))*10600000, adjust = 1), 
#       col = "red", lty = 2)
# plot(density(exp(chain$beta_u)*10600000, adjust = 1), bty = "n",
#      ylab = "Density", xlab = expression(beta[u]), main = "")
# lines(density(exp(rnorm(100000, log(5e-8), 0.4))*10600000, adjust = 1), 
#       col = "red", lty = 2)
# plot(density(exp(chain$beta_e)*10600000, adjust = 1), bty = "n",
#      ylab = "Density", xlab = expression(beta[e]), main = "")
# lines(density(exp(rnorm(100000, log(5e-8), 0.4))*10600000, adjust = 1), 
#       col = "red", lty = 2)
# plot(density(chain$beta_red_factor, adjust = 1), bty = "n",
#      ylab = "Density", xlab = expression(xi), main = "")
# lines(x = seq(0, 1, by = 0.01), dunif(x = seq(0, 1, by = 0.01), 0.01, 1), 
#       col = "red", lty = 2)
# plot(density(chain$gamma_u), bty = "n", ylab = "Density", 
#      xlab = expression(gamma[u]), main = "")
# lines(density(rlnorm(100000, log(0.5), 1)), 
#       col = "red", lty = 2)
# plot(density(chain$gamma_d), bty = "n", ylab = "Density", 
#      xlab = expression(gamma[d]), main = "")
# lines(density(rlnorm(100000, log(0.5), 1)), 
#       col = "red", lty = 2)
# plot(density(chain$detect_frac_0, adjust = 1), bty = "n",
#      ylab = "Density", xlab = "detect_frac_0", main = "")
# lines(x = seq(0, 1, by = 0.01), dunif(x = seq(0, 1, by = 0.01), 0.01, 0.6), 
#       col = "red", lty = 2)
# plot(density(chain$theta), bty = "n", xlab = expression(theta), main= "")

      