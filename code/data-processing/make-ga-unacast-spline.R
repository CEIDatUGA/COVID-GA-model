# load libraries
library(tidyr)
library(dplyr)
library(magrittr)

# Create full time series of NAs to merge with the UNACAST data
pseudo_data <- data.frame(
  Date = seq.Date(from = as.Date("2020-02-01"), to = Sys.Date(), by = "day"),
  hold = NA)


# Load the data -----------------------------------------------------------     
state_data <- read.csv(file = "unacast/sds-v3-full-state.csv")

# loop through for each state 
ga <- filter(state_data, state_name == "Georgia")
unacast <- ga  %>%
  dplyr::select(date, daily_distance_diff) %>%
  rename("Date" = date, "rel_beta_change" = daily_distance_diff) %>%
  mutate(Date = as.Date(Date)) %>%
  right_join(pseudo_data, by = "Date") %>%
  dplyr::select(-hold) %>%
  fill(rel_beta_change) %>%  # fills NAs with last observed value
  mutate(time = 1:n()) %>%
  dplyr::select(Date, time, rel_beta_change) %>%
  mutate(rel_beta_change = ifelse(sign(rel_beta_change) == 1, 
                                  rel_beta_change + 1, 
                                  1 - abs(rel_beta_change))) %>%
  mutate(rel_beta_change = ifelse(is.na(rel_beta_change), 1, rel_beta_change))

mod <- smooth.spline(x = unacast$time, 
                     y = unacast$rel_beta_change, 
                     spar = 0.6)
pred <- predict(mod)



# Save the output ---------------------------------------------------------

covar_table <- data.frame(Date = unacast$Date,
                          time = pred$x,
                          rel_beta_change = pred$y)
saveRDS(covar_table, file = paste("data/rel-beta-change-covar.rds"))
