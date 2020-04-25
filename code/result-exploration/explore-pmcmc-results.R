# explore-pmcmc-results.R
# This script loads results produced by run-pmcmc for exploration/plotting

# Clear the decks ---------------------------------------------------------
rm(list = ls(all.names = TRUE))

# Load libraries ----------------------------------------------------------
library(dplyr)
library(pomp)
library(here)


# Load the pmcmc output list ---------------------------
mcmcs <- readRDS(here("output/pmcmc-output.RDS"))

# Load the pomp object ----------------------------------------------------
filename <- here('output/pomp-model.RDS')
pomp_model <- readRDS(filename)


# Define a few global settings regarding dates and observations -----------

start_date <- as.Date("2020-03-01")  # start of simulation/fitting
end_date <- start_date + max(time(mcmcs[[1]]))  # end of forecast
dates <- seq.Date(start_date, end_date, "days")   # all the dates

# Make a data frame that maps 'time' to 'Date'
dates_df <- data.frame(time = c(1:length(dates)), 
                       Date = dates)

# Read in the observation data associated with pomp model object
# and reformat into a long data frame.
dat <- t(pomp_model@data) %>%  
  as.data.frame() %>%
  mutate(time = 1:n()) %>%
  left_join(dates_df, by = "time")  # add in dates



