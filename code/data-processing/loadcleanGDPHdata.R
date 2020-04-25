loadcleanGDPHdata <- function(use_these_locations, start_date = "2020-03-01")
{
  # load and clean/process covid tracking data so it is ready for fitting
  # Load libraries ----------------------------------------------------------
  library(dplyr)
  library(readr)

    #################################
    # pull data from Covidtracking and process
    #################################
    us_data <- read_csv("https://raw.githubusercontent.com/CEIDatUGA/COVID-19-DATA/master/georgia/ga_GDPH_daily_status_report/GA_daily_status_report_GDPH.csv")
      us_clean <- us_data %>% dplyr::select(c(date,new_cases,new_hospitalizations,new_fatalities)) %>%
      rename(Date = date,
             cases = new_cases,
             hosps = new_hospitalizations, 
             deaths = new_fatalities) 
             
    
    us_clean[us_clean < 0] <- 0 #set negative values to 0
    
    us_clean$hosps <- NA #set to NA    
    
    pseudo_data <- data.frame(
      Date = seq.Date(from = as.Date(start_date), to = max(us_clean$Date), by = "day"),
      hold = NA)
    
    pomp_data <- us_clean %>% 
      arrange(Date) %>%
      right_join(pseudo_data, by = "Date") %>%
      dplyr::select(-hold) %>%
      mutate(time = 1:n())

    return(pomp_data)    
}