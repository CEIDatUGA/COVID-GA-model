# format-forecasts.R
# This script formats forecasts made by the run-particle-mcmc.R script.
# Forecasts are formatted in two ways:
#   1. A data frame of new hospitalizations indexed by simulation rep
#      and date.
#   2. A list of data frames, where each data frame is the output from a 
#      single simulation rep for all variables over the simulate dates.


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))

# SAVE_RESULTS <- FALSE
# MAKE_PLOTS <- TRUE


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(pomp)


# Load the simulations ----------------------------------------------------

most_recent_files <- tail(list.files(path = here("output"), "Georgia_COV"),3)
filename_sims <- most_recent_files[grep(pattern = "simulation-scenarios", most_recent_files)]

filename_sims <- "Georgia_COV_2020-05-11_simulation-scenarios.rds"  # for specific runs

all_sims <- readRDS(here("output", filename_sims))
forecasts <- all_sims %>%
  filter(SimType == "status_quo") %>%
  filter(Period == "Future") %>%
  mutate(Rep = as.numeric(as.factor(paste(.id, mle_id, sep = "-")))) %>%
  dplyr::select(Rep, Date, cases, deaths) %>%
  arrange(Rep, Date) %>%
  as.data.frame()
# ggplot(forecasts, aes(x = Date, y = deaths)) +
#   geom_line(aes(group = Rep), alpha = 0.2)

fore_date <- min(forecasts$Date)
filename <- paste0("output/forecasts/forecasts-", fore_date, ".rds")
saveRDS(object = forecasts,
        file = here(filename))



# # Load the simulations ----------------------------------------------------
# 
# all_sims <- readRDS(here("output/Georgia_COV_2020-04-30-11-23_simulation-scenarios.rds"))
# hosps_forecasts <- all_sims %>%
#   filter(SimType == "status_quo") %>%
#   filter(Date >= Sys.Date()) %>%
#   mutate(Rep = as.numeric(as.factor(paste(.id, mle_id, sep = "-")))) %>%
#   dplyr::select(Rep, Date, hosps) %>%
#   rename("H_new" = hosps) %>%
#   arrange(Rep, Date) %>%
#   as.data.frame()
# 
# # hosps_forecasts %>%
# #   group_by(Date) %>%
# #   summarise(lower = quantile(H_new, 0.1),
# #             med = median(H_new),
# #             upper = quantile(H_new, 0.9)) %>%
# #   ggplot(aes(x = Date, y = med)) +
# #   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
# #   geom_line()
# 
# filename <- paste0("output/hospital-forecasts/hosps-forecasts-", Sys.Date(), ".rds")
# saveRDS(object = hosps_forecasts,
#         file = filename)
# # 
# # 
# # 
# # 
# # # Load the pomp model and the pmcmc output list ---------------------------
# # 
# # pomp_model <- readRDS(here("output/pomp-model.RDS"))
# # mcmcs <- readRDS(here("output/pmcmc-output.RDS"))
# # 
# # 
# # # Define a few global settings regarding dates and observations -----------
# # 
# # start_date <- as.Date("2020-03-01")  # start of simulation/fitting
# # end_date <- start_date + max(time(mcmcs[[1]]))  # end of forecast
# # dates <- seq.Date(start_date, end_date, "days")   # all the dates
# # 
# # # Make a data frame that maps 'time' to 'Date'
# # dates_df <- data.frame(time = c(1:length(dates)), 
# #                        Date = dates)
# # 
# # # Read in the observation data associated with pomp model object
# # # and reformat into a long data frame.
# # dat <- t(pomp_model@data) %>%  
# #   as.data.frame() %>%
# #   mutate(time = 1:n()) %>%
# #   left_join(dates_df, by = "time")  # add in dates
# # 
# # 
# # # Collate for the forecast competition ------------------------------------
# # 
# # hosps_forecasts <- tibble()  # empty storage object
# # 
# # # Loop over the pMCMC chains and extract new hospitalization estimates
# # # and forecasts.
# # for(i in 1:length(mcmcs)) {
# #   tmp <- mcmcs[[i]]  # the pmcmc object
# #   tmp <- tmp@filter.traj  # the filtered trajectories for each MCMC iteration
# #   
# #   # Reshape the large matrix into a long dataframe indexed by
# #   # time, chain, and iteration
# #   tmp <- t(tmp["H_new", , ]) %>%  # only use 1000 final iterations
# #     as.data.frame() %>%
# #     gather(key = "iter", value = "H_new") %>%
# #     group_by(iter) %>%
# #     mutate(time = 1:n()) %>%
# #     ungroup() %>%
# #     mutate(chain = i,  # add chain id
# #            iter = as.numeric(iter)) %>%
# #     left_join(dates_df, by = "time") %>%
# #     dplyr::select(chain, iter, Date, H_new)  # just keep relevant variables
# #   
# #   # Bind the rows for storage
# #   hosps_forecasts <- bind_rows(hosps_forecasts, tmp)
# # }
# # 
# # # Final formatting for the forecasting team
# # hosps_forecasts <- hosps_forecasts %>%
# #   mutate(Rep = as.numeric(as.factor(paste(chain, iter)))) %>%
# #   dplyr::select(Rep, Date, H_new)
# # 
# # # Save the object for forecasting team to format for submission
# # if(SAVE_RESULTS) {
# #   filename <- paste0("output/hosps-forecasts-", Sys.Date(), ".RDS")
# #   saveRDS(object = as.data.frame(hosps_forecasts),
# #           file = filename)
# # }
# # 
# # 
# # # Make some forecast summaries and figures --------------------------------
# # 
# # if(MAKE_PLOTS) {
# #   hosps_summary <- hosps_forecasts %>%
# #     group_by(Date) %>%
# #     summarise(lower = quantile(H_new, 0.025),
# #               med = median(H_new),
# #               upper = quantile(H_new, 0.975)) %>%
# #     ungroup() %>%
# #     mutate(Period = ifelse(Date > Sys.Date(), "Forecast", "Calibration"))
# #   
# #   two_week_days <- 16  # 14 days plus 2 to account for lag from Thurs. to the Sat.
# #   p1 <- ggplot(hosps_summary %>% filter(Date < (Sys.Date() + 16)), 
# #                aes(x = Date, y = med)) +
# #     geom_ribbon(aes(ymin = lower, ymax = upper, fill = Period),
# #                 alpha = 0.2, color = NA) +
# #     geom_line(aes(color = Period)) +
# #     geom_point(data = dat, aes(y = hosps)) +
# #     labs(y = "Number of new hospitalizations") +
# #     ggtitle("2-week forecast")
# #   
# #   p2 <- ggplot(hosps_summary, aes(x = Date, y = med)) +
# #     geom_ribbon(aes(ymin = lower, ymax = upper, fill = Period),
# #                 alpha = 0.2, color = NA) +
# #     geom_line(aes(color = Period)) +
# #     geom_point(data = dat, aes(y = hosps)) +
# #     labs(y = "Number of new hospitalizations") +
# #     ggtitle("7-week forecast")
# #   
# #   cowplot::plot_grid(p1, p2, ncol = 2)
# # }
# # 
# # 
# # # Format full forecast results --------------------------------------------
# # 
# # all_forecasts <- list()
# # count <- 1  # list index counter
# # for(i in 1:length(mcmcs)) {
# #   tmp <- mcmcs[[i]]  # the pmcmc object
# #   tmp <- tmp@filter.traj  # the filtered trajectories for each MCMC iteration
# #   tmp <- tmp[ , 1001:2000, ]
# #   
# #   for(j in 1:dim(tmp)[2]) {
# #     all_forecasts[[count]] <- t(tmp[ , j , ]) %>% 
# #       as.data.frame() %>%
# #       mutate(time = 1:n()) %>%
# #       left_join(dates_df, by = "time") 
# #     
# #     count <- count + 1  # advance list index counter
# #   }  # end iteration loop
# #   
# # }  # end chain loop
# # 
# # # Save the results
# # if(SAVE_RESULTS) {
# #   filename <- paste0("output/all-states-forecasts-", Sys.Date(), ".RDS")
# #   saveRDS(object = as.data.frame(hosps_forecasts),
# #           file = filename)
# # }
# # 
