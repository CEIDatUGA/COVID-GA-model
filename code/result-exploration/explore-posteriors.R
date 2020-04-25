# analyze-posteriors.R

library(tidyverse)
library(pomp)
library(here)



# Load the pmcmc objects and the priors -----------------------------------

mcmcs <- readRDS(here("output/pmcmc-output.RDS"))
prior_means <- readRDS(here("output/parvals.RDS"))
prior_sds <- c(rep(0.2, 21), 5,5,5,5,
               4,4,4,4, 7,7,7,7, 3,3,3,3, 0,0,0,
               0,0,0,0,0,0,0,0,0,0)

# Combine MCMC chains and get posterior distributions ---------------------

all_chains <- tibble()
for(i in 1:length(mcmcs)) {
  chain <- mcmcs[[i]]@traces
  keeps <- tail(seq_along(chain[,1]), 1000)
  chain <- chain[keeps, ] %>%
    as.data.frame() %>%
    mutate(chain = i,
           iteration = 1:n())
  all_chains <- bind_rows(all_chains, chain)
}

all_chains <- all_chains %>%
  dplyr::select(-loglik, -log.prior, -chain, -iteration)


# Calculate prior distributions for each parameter ------------------------

priors_posts <- tibble()
for(i in 1:ncol(all_chains)) {
  tmp <- all_chains[ , i] %>%
    deframe()
  param_name <- names(all_chains)[i]
  # x <- seq(min(tmp), max(tmp), length.out = length(tmp))
  priors <- rnorm(length(tmp), prior_means[param_name], prior_sds[i])
  outtb <- tibble(Parameter = names(all_chains)[i],
                  Prior = priors,
                  Posterior = tmp)
  priors_posts <- bind_rows(priors_posts, outtb)
}

priors_posts %>%
  filter(!Parameter %in% c("t_int1", "t_int2", "t_int3", "S_0",
                           "C1_0", "C2_0", "C3_0", "C4_0",
                           "D_0", "R_0", "H1_0", "H2_0", "H3_0", "H4_0")) %>%
  gather("key", "value", -Parameter) %>%
  ggplot(aes(x = value, color = key, linetype = key)) +
  geom_density() +
  facet_wrap(~Parameter, scales = "free") +
  scale_color_manual(values = c("black", "red")) +
  ggthemes::theme_few()

