loadcleanGDPHdata_v2 <- function(use_these_locations, start_date)
{
  # load and clean/process covid tracking data so it is ready for fitting
  # Load libraries ----------------------------------------------------------
  library(dplyr)
  library(readr)

    #################################
    # pull data from Covidtracking and process
    #################################
    us_data <- read_csv("https://raw.githubusercontent.com/CEIDatUGA/COVID-19-DATA/master/georgia/ga_GDPH_daily_status_report/GA-DPH-CanvasJS-data-cases-deaths.csv")
      us_clean <- us_data %>% dplyr::mutate( cases = c(0,diff(cumulative_cases)),
                                             hosps = NA,
                                             deaths = c(0,diff(cumulative_deaths))
                                             ) %>% 
                             dplyr::select(c(date,cases,hosps,deaths)) %>%
                             rename(Date = date) 
             
    pseudo_data <- data.frame(
      Date = seq.Date(from = as.Date(start_date), to = Sys.Date(), by = "day"),
      hold = NA)
    
    pomp_data <- us_clean %>% 
      arrange(Date) %>%
      right_join(pseudo_data, by = "Date") %>%
      dplyr::select(-hold) %>%
      mutate(time = 1:n())

    return(pomp_data)    
}