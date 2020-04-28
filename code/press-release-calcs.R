# 1a. Compare total number reported cases between data and no intervention scenario
#    up to April 26 
# 1b. Compare total number reported deaths between data and no intervention scenario
#    up to April 26 
# 2a. Compare cumulative number of cases from today to end of forecast between relaxation
#     and increasing social dist
# 2b. Compare cumulative number of deaths from today to end of forecast between relaxation
#     and increasing social dist
# 3a. Cumulative cases from no intervention by June 6
# 3b. Cumulative deaths from no intervention by June 6


library(tidyverse)
library(here)


# Read in the simulations -------------------------------------------------

# Can set filename_label manually, if needed
filename_label <- "Georgia_COV_2020-04-27-12-55"
filename_mif <- here('output', paste0(filename_label, '_mif.rds'))
simfile <- here('output', paste0(filename_label, '_simulation-scenarios.rds'))
covarfile <- here('output', paste0(filename_label, '_simulation-covariates.rds'))
out_sims <- readRDS(simfile)
covar_scens <- readRDS(covarfile)
pomp_model <- readRDS(here("output/pomp-model.RDS"))


# Summarize the simulations -----------------------------------------------
# dailies
sim_summs <- out_sims %>%
  dplyr::select(SimType, Period, Date, cases, hosps, deaths) %>%
  gather(key = "Variable", value = "Value", -SimType, -Period, -Date) %>%
  group_by(SimType, Period, Date, Variable) %>%
  summarise(ptvalue = ceiling(mean(Value))) %>%
  ungroup() 

# cumulatives
cumulative_summs <- out_sims %>%
  dplyr::select(SimType, Date, cases, hosps, deaths, rep_id) %>%
  gather(key = "Variable", value = "Value", -SimType, -Date, -rep_id) %>%
  arrange(SimType, Variable, rep_id, Date) %>%
  group_by(SimType, Variable, rep_id) %>%
  mutate(Value = cumsum(Value)) %>%
  group_by(SimType, Variable, Date) %>%
  summarise(ptvalue = ceiling(mean(Value))) %>%
  ungroup() 

# data
end_date <- as.Date("2020-04-26")  # last day of data
dates <- seq.Date(as.Date("2020-03-01"), end_date, "days") 
dates_df <- data.frame(time = c(1:length(dates)), Date = dates)
pomp_data <- pomp_model@data %>%
  t() %>%
  as.data.frame() %>%
  dplyr::mutate(time = 1:n()) %>%
  right_join(dates_df, by = "time") %>%
  dplyr::select(Date, cases, hosps, deaths) %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  mutate(SimType = "obs", Period = "Past")

 


# Make the calculations ---------------------------------------------------

# 1a. Compare total number reported cases between data and no intervention scenario
#    up to April 26 
pomp_data %>%
  filter(Variable == "cases") %>%
  filter(Date <= "2020-04-26") %>%
  pull(Value) %>%
  sum(na.rm = TRUE) -> total_cases_data

cumulative_summs %>%
  filter(SimType == "no_intervention") %>%
  filter(Variable == "cases") %>%
  filter(Date == "2020-04-26") %>%
  pull(ptvalue) -> total_cases_noint

1 - (total_cases_data / total_cases_noint)  # % reduction in cases


# 1b. Compare total number reported deaths between data and no intervention scenario
#    up to April 26 
pomp_data %>%
  filter(Variable == "deaths") %>%
  filter(Date <= "2020-04-26") %>%
  pull(Value) %>%
  sum(na.rm = TRUE) -> total_deaths_data

cumulative_summs %>%
  filter(SimType == "no_intervention") %>%
  filter(Variable == "deaths") %>%
  filter(Date == "2020-04-26") %>%
  pull(ptvalue) -> total_deaths_noint

# sim_summs %>%
#   filter(SimType == "no_intervention") %>%
#   filter(Variable == "deaths") %>%
#   filter(Date <= "2020-04-26") %>%
#   pull(ptvalue) %>% sum()

total_deaths_noint - total_deaths_data  # lives saved


# 2a. Compare cumulative number of cases from today to end of forecast between relaxation
#     and increasing social dist
cumulative_summs %>%
  filter(SimType == "linear_decrease_sd") %>%
  filter(Variable == "cases") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_cases_proj_relax

cumulative_summs %>%
  filter(SimType == "linear_increase_sd") %>%
  filter(Variable == "cases") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_cases_proj_increase

total_cases_proj_relax - total_cases_proj_increase - total_cases_data


# 2b. Compare cumulative number of deaths from today to end of forecast between relaxation
#     and increasing social dist
cumulative_summs %>%
  filter(SimType == "linear_decrease_sd") %>%
  filter(Variable == "deaths") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_deaths_proj_relax

cumulative_summs %>%
  filter(SimType == "linear_increase_sd") %>%
  filter(Variable == "deaths") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_deaths_proj_increase

total_deaths_proj_relax - total_deaths_proj_increase - total_deaths_data


# 3a. Cumulative cases from no intervention by June 7
cumulative_summs %>%
  filter(SimType == "no_intervention") %>%
  filter(Variable == "cases") %>%
  filter(Date == max(Date)) %>%
  pull(ptvalue)

# 3b. Cumulative deaths from no intervention by June 7
cumulative_summs %>%
  filter(SimType == "no_intervention") %>%
  filter(Variable == "deaths") %>%
  filter(Date == max(Date)) %>%
  pull(ptvalue)

# 4a. Compare cumulative number of cases from today to end of forecast between relaxation
#     and status quo
cumulative_summs %>%
  filter(SimType == "linear_decrease_sd") %>%
  filter(Variable == "cases") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_cases_proj_relax

cumulative_summs %>%
  filter(SimType == "status_quo") %>%
  filter(Variable == "cases") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_cases_proj_statquo

total_cases_proj_relax - total_cases_proj_statquo - total_cases_data

# 4b. Compare cumulative number of deaths from today to end of forecast between relaxation
#     and status quo
cumulative_summs %>%
  filter(SimType == "linear_decrease_sd") %>%
  filter(Variable == "deaths") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_deaths_proj_relax

cumulative_summs %>%
  filter(SimType == "status_quo") %>%
  filter(Variable == "deaths") %>%
  filter(Date == max(Date)) %>%
  pull() -> total_deaths_proj_statquo

total_deaths_proj_relax - total_deaths_proj_statquo - total_deaths_data


# Total cases, hosps, and deaths from March 1, all scenarios
# and total from April 26.
pomp_data %>%
  group_by(Variable) %>%
  summarize(ValTot = sum(Value, na.rm = TRUE)) %>%
  ungroup() -> obs_totals

scenarios <- c("linear_decrease_sd", 
               "linear_increase_sd",
               "return_normal",
               "status_quo")
cumulative_summs %>%
  filter(SimType %in% scenarios) %>%
  filter(Date == max(Date)) %>%
  left_join(obs_totals, by = "Variable") %>%
  mutate(`Total From April 26` = ptvalue - ValTot) %>%
  mutate(`Total From March 1` = ptvalue) %>%
  dplyr::select(SimType, Variable, `Total From March 1`, `Total From April 26`) %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "Relax social distancing", SimType),
         SimType2 = ifelse(SimType == "no_intervention", "No intervention", SimType2),
         SimType2 = ifelse(SimType == "lowest_sd", "Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "Status quo", SimType2),
         SimType2 = ifelse(SimType == "linear_increase_sd", "Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2) %>%
  knitr::kable(format = "markdown")

cumulative_summs %>%
  filter(SimType %in% scenarios) %>%
  filter(Date == max(Date)) %>%
  left_join(obs_totals, by = "Variable") %>%
  mutate(`Total From April 26` = ptvalue - ValTot) %>%
  mutate(`Total From March 1` = ptvalue) %>%
  dplyr::select(SimType, Variable, `Total From March 1`, `Total From April 26`) %>%
  mutate(SimType2 = ifelse(SimType == "linear_decrease_sd", "Relax social distancing", SimType),
         SimType2 = ifelse(SimType == "no_intervention", "No intervention", SimType2),
         SimType2 = ifelse(SimType == "lowest_sd", "Continuously improving social distancing", SimType2),
         SimType2 = ifelse(SimType == "status_quo", "Status quo", SimType2),
         SimType2 = ifelse(SimType == "linear_increase_sd", "Increased social distancing", SimType2),
         SimType2 = ifelse(SimType == "return_normal", "Return to normal", SimType2)) %>%
  mutate(SimType = SimType2) %>%
  dplyr::select(-SimType2) %>%
  write_csv("../../Desktop/totals_for_jmd.csv")
