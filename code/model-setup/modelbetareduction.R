modelbetareduction <- function()
{  
  # model-beta-reduction.R
  # This function fits a smoothing spline model to UNACAST data on
  # social distancing. These relative changes in behavior are used to
  # scale the baseline transmission rate over time.
  #
  # ************************************************************
  # NOTE: These data are private and are thus not held on this
  #       repository. This script can only be run by those with
  #       access to the private data. The derived model values,
  #       however, are stored on the repo for modeling.
  # ************************************************************
  library(tidyr)
  library(dplyr)
  
  # Create full time series of NAs to make sure pomp_data
  # starts at the time of model initialization
  pseudo_data <- data.frame(
    Date = seq.Date(from = as.Date("2020-02-01"), to = Sys.Date(), by = "day"),
    hold = NA)
  
  
  # Load the data -----------------------------------------------------------     
  
  # unacast <- read.table(here("data/unacast-ga-private.txt"), header = TRUE) %>%
  filename <- list.files(path = here("data/"), pattern = "ga_state_raw")
  filename <- paste0("data/", filename)
  unacast <- read.csv(here(filename))  %>%
    dplyr::select(date, daily_distance_diff) %>%
    rename("Date" = date, "rel_beta_change" = daily_distance_diff) %>%
    # separate(Date, into = c("m", "d", "y")) %>%
    # mutate(m = str_pad(m, width = 2, side = "left", pad = "0"),
    #        d = str_pad(d, width = 2, side = "left", pad = "0"),
    #        y = "2020") %>%
    # mutate(Date = as.Date(paste(y, m, d, sep = "-"))) %>%
    # dplyr::select(-y, -m, -d) %>%
    mutate(Date = as.Date(Date)) %>%
    right_join(pseudo_data, by = "Date") %>%
    dplyr::select(-hold) %>%
    fill(rel_beta_change) %>%  # fills NAs with last observed value
    mutate(time = 1:n()) %>%
    dplyr::select(time, rel_beta_change) %>%
    mutate(rel_beta_change = ifelse(sign(rel_beta_change) == 1, 
                                    rel_beta_change + 1, 
                                    1 - abs(rel_beta_change))) %>%
    mutate(rel_beta_change = ifelse(is.na(rel_beta_change), 1, rel_beta_change))
  
  mod <- smooth.spline(x = unacast$time, y = unacast$rel_beta_change, spar = 0.6)
  pred <- predict(mod)
  # plot(unacast$rel_beta_change, pch = 19, xlab = "Days", ylab = "Relative change in transmission")
  # lines(pred, col = "red")
  
  
  # Save the output for pomp ------------------------------------------------     
  
  covar_table <- data.frame(time = pred$x,
                            rel_beta_change = pred$y) %>%
    mutate(rel_beta_change = ifelse(rel_beta_change > 1, 1, rel_beta_change))
  saveRDS(covar_table, file = here("output/rel-beta-change-covar.RDS"))

}
