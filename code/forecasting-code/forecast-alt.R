# forecast-alt.R
# Alternate forecasting script using the King et al. ebola example
# approach. It is empirical Bayes, simulating from parameter values
# around the MLEs identified by MIF.


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)
library(here)
library(foreach)
library(doParallel)
library(iterators)


# Load the MIF results ----------------------------------------------------

filename <- (here("output/mif-results.RDS"))
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
logliks <- ll_df %>%
  dplyr::rename("LogLik" = X1,
                "LogLik_SE" = X2) %>%
  dplyr::mutate(MIF_ID = 1:n()) %>%
  dplyr::select(MIF_ID, LogLik, LogLik_SE) %>%
  bind_cols(mif_coefs) %>%
  dplyr::arrange(-LogLik)


# Define some global functions and settings -------------------------------

# Forecast horizon
horizon <- 7 * 7  # 7 days * 7 weeks

# Weighted quantile function (from King et al. 2015)
wquant <- function (x, weights, probs = c(0.025,0.5,0.975)) {
  idx <- order(x)
  x <- x[idx]
  weights <- weights[idx]
  w <- cumsum(weights)/sum(weights)
  rval <- approx(w,x,probs,rule=1)
  rval$y
}



# Define parameter vectors around the MLEs --------------------------------


logliks  %>%
  select(-MIF_ID,-LogLik_SE) %>%
  # filter(LogLik > max(LogLik) - 0.5*qchisq(df=1, p=0.99)) %>%
  gather(parameter,value) %>%
  group_by(parameter) %>%
  summarize(min=min(value),max=max(value)) %>%
  ungroup() %>%
  filter(parameter!="LogLik") %>%
  column_to_rownames("parameter") %>%
  as.matrix() -> ranges

sobolDesign(lower=ranges[,'min'],
            upper=ranges[,'max'],
            nseq=20) -> params

params <- logliks[,4:ncol(logliks)] %>%
  as.data.frame()

# Run the simulations -----------------------------------------------------
# 
# registerDoParallel()
# registerDoRNG(887851050L)

foreach(p=iter(params,by='row'),
        .inorder=FALSE,
        .combine=bind_rows
) %do% {
  
  library(pomp)
  
  M1 <- readRDS(here("output/pomp-model.RDS"))
  
  M1 %>% pfilter(params=p,Np=2000,save.states=TRUE) -> pf
  
  pf@saved.states %>%               # latent state for each particle
    tail(1) %>%                     # last timepoint only
    melt() %>%                      # reshape and rename the state variables
    spread(variable,value) %>%
    group_by(rep) %>%
    summarize(
      S_0=S,
      E1_0=log(E1), #E2_0=E2, E3_0=E3, E4_0=E4,
      Ia1_0=log(Ia1), #Ia2_0=Ia2, Ia3_0=Ia3, Ia4_0=Ia4,
      Isu1_0=log(Isu1), #Isu2_0=Isu2, Isu3_0=Isu3, Isu4_0=Isu4,
      Isd1_0=log(Isd1),#Isd2_0=Isd2, Isd3_0=Isd3, Isd4_0=Isd4,
      C1_0 = C1, #C2_0 = C2, C3_0 = C3, C4_0 = C4,
      H1_0 = H1, #H2_0 = H2, H3_0 = H3, H4_0 = H4,
      R_0=R, D_0 = D
    ) %>%
    gather(variable,value,-rep) %>%
    spread(rep,value) %>%
    column_to_rownames("variable") %>%
    as.matrix() -> x
  ## the final states are now stored in 'x' as initial conditions
  
  ## set up a matrix of parameters
  pp <- parmat(unlist(p),ncol(x))
  
  ## generate simulations over the interval for which we have data
  M1 %>%
    simulate(params=pp,format="data.frame") %>%
    select(.id, time, H_new, hosps, cases, C_new, D_new, deaths) %>%
    mutate(
      period="calibration",
      loglik=logLik(pf)
    ) -> calib
  
  ## make a new 'pomp' object for the forecast simulations
  M2 <- M1
  time(M2) <- max(time(M1))+seq_len(horizon)
  timezero(M2) <- max(time(M1))
  
  # Set covars table for forecasts
  newcovars <- data.frame(
    rel_beta_change = rep(tail(t(M1@covar@table), 1), times = horizon +1)) %>%
    mutate(time = 1:n() + (max(time(M1)-1)))
  M2@covar <- covariate_table(newcovars, times = "time", order = "constant")
  
  ## set the initial conditions to the final states computed above
  pp[rownames(x),] <- x
  
  ## perform forecast simulations
  M2 %>%
    simulate(params=pp,format="data.frame") %>%
    select(.id, time, H_new, hosps, cases, C_new, D_new, deaths) %>%
    mutate(
      period="projection",
      loglik=logLik(pf)
    ) -> proj
  
  bind_rows(calib,proj)
} -> out

out %>%
  # filter(loglik != -Inf) %>%
  mutate(weight=exp(loglik-mean(loglik))) %>%
  arrange(time,.id) -> sims


## look at effective sample size
# ess <- with(subset(sims,week==max(week)),weight/sum(weight))
# ess <- 1/sum(ess^2); ess



## compute quantiles of the forecast incidence
sims %>%
  # mutate(weight = ifelse(weight == Inf, 1, weight)) %>%
  group_by(time,period) %>%
  # summarize(
  #   lower = quantile(hosps, probs = 0.025),
  #   median = mean(hosps),
  #   upper = quantile(hosps, probs = 0.975)
  # ) %>%
  summarize(
    lower=wquant(cases,weights=weight,probs=0.025),
    median=wquant(cases, weights=weight,probs=0.5),
    upper=wquant(cases,weights=weight,probs=0.975)
  ) %>%
  ungroup() -> simq

# thedata <- readRDS(here("output/pomp-model.RDS"))@data
ggplot(simq, aes(x = time, y = median, color = period)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line() +
  xlab("Days since March 1") + ylab("New hospitalizations")

start_date <- as.Date("2020-03-01")
end_date <- start_date + max(sims$time) - 1
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(Time = c(1:length(dates)), Date = dates)

# sims %>%
#   rename("Time" = time, "H_new" = H1, "rep" = .id) %>%
#   dplyr::select(Time, H_new, rep) %>%
#   left_join(dates_df, by = "Time") %>%
#   dplyr::select(Date, H_new, rep) -> forecasts

# saveRDS(object = forecasts, file = here("output/forecasts.RDS"))


ggplot(calib, aes(x = time, y = hosps, group = .id)) +
  geom_line(alpha = 0.1) +
  geom_hline(aes(yintercept = 180), color = "red")
