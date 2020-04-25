# simulate-pomp-model.R
#
# This script runs the previously generated pomp model
# it does not do fitting, but can be used for exploration 
# and to generate generate synthetic data

rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(pomp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here) #to simplify loading/saving into different folders

# Load the pomp object ----------------------------------------------------
filename <- here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)

#load the data used to build the pomp model
filename <- here('data',paste0("us-ct-cleandata-",Sys.Date(),'.rds'))
pomp_data <- readRDS(filename)


# load values for model parameters and initial conditions -----------------
filename = here('output/var-par-definitions.RDS')
par_var_list <- readRDS(filename) 
allparvals <- par_var_list$allparvals
params_to_estimate = par_var_list$params_to_estimate
inivals_to_estimate = par_var_list$inivals_to_estimate


# load simulation/prediction runs with model parameters specified in the simulate-pomp script -----------------
filename = here('output/model-predictions.RDS')
sims <- readRDS(filename) 


# prep simulation results for plotting -----------------
plot_dat <- sims %>%
  as_tibble() %>%
  dplyr::select(Date, .id, S, Iatot, Isutot, Isdtot, Ctot, Htot, R, D, C_new, H_new, D_new, cases, hosps, deaths) %>%
  rename("New Cases" = cases, "New Mortality" = deaths, "New Hospitalizations" = hosps) %>%
  mutate(Period = ifelse(Date < Sys.Date(), "Calibration", "Forecast")) %>%
  tidyr::gather(key = "variable", value = "value", -Date, -.id, -Period) %>%  #change to long format
  filter(.id != "data") %>%
  group_by(Date, variable, Period) %>%
  summarise(lower = ceiling(quantile(value, 0.1)),
            medvalue = ceiling(mean(value)),
            upper = ceiling(quantile(value, 0.9))) %>%
  ungroup()

# make a data frame containing data only -----------------
# used in plots below
datadf <- pomp_data %>%
  as_tibble() %>%
  rename("New Cases" = cases, "New Mortality" = deaths, "New Hospitalizations" = hosps) %>%
  mutate(Period = "Calibration") %>%
  tidyr::gather(key = "variable", value = "value", -Date, -Period, -time, -Location) 
  

# plot all model compartments and data ---------------------
pl_all <- plot_dat %>% 
  ggplot(aes(x = Date, y = medvalue)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(size = 1, aes(linetype = Period)) +
  geom_point(data = datadf, aes(x = Date, y = value), color = "red") +
  facet_wrap(~variable, scales = "free_y") +
  scale_color_manual(values = c("#beaed4"))+
  scale_fill_manual(values = c("#beaed4"))+
  theme_minimal() +
  guides(color = FALSE, fill = FALSE, linetype = FALSE) +
  ylab("")

plot(pl_all)
filename = here('output/figures/traj-plot.png')
ggsave(filename,pl_all)


# nice plot showing cases, hospitalizations and deaths  -----------------
plh <- plot_dat %>% 
  dplyr::filter(variable %in% c("New Cases", "New Mortality", "New Hospitalizations")) %>%
  ggplot(aes(x = Date, y = medvalue, color = Period, fill = Period)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line() +
  geom_point(data = datadf, aes(x = Date, y = value), color = "grey35") +
  facet_wrap(~variable, scales = "free_y") 

plot(plh)
filename = here('output/figures/fit-plot.png')
ggsave(filename,plh)


# some more explorations, just checks, not otherwise useful
if (1==2)
{
x <- sims %>%
  as_tibble() %>%
  dplyr::select(Date, .id, S, Iatot, Isutot, Isdtot, Ctot, Htot, R, D, C_new, H_new, D_new, cases, hosps, deaths) %>%
  mutate(Period = ifelse(Date < Sys.Date(), "Calibration", "Forecast")) %>%
  filter(.id != "data") %>%   
  group_by(.id) %>%
  mutate(D_comp = cumsum(D_new)) %>%
  ungroup() %>%
  select(Date, .id, D, D_new, D_comp) %>% 
  tidyr::gather(key = "variable", value = "value", -Date, -.id)  #change to long format
  
pl <- x %>% 
  ggplot(aes(x = Date, y = value,  group = .id)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") 

plot(pl)
}
  