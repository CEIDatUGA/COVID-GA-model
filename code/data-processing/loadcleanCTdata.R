loadcleanCTdata <- function(use_these_locations, start_date = "2020-03-01")
{

  # load and clean/process covid tracking data so it is ready for fitting
  # Load libraries ----------------------------------------------------------
   library(dplyr)
   library(readr)

    #################################
    # pull data from Covidtracking and process
    #################################
    us_data <- read_csv("https://covidtracking.com/api/states/daily.csv")
    #data for population size for each state/country so we can compute cases per 100K
    #not currently needed, but keep here just in case
    filename = here('data/us_popsize.rds')
    us_popsize <- readRDS(filename)
    us_clean <- us_data %>% dplyr::select(c(date,state,positive,negative,total,hospitalized,death)) %>%
      mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
      group_by(state) %>% arrange(date) %>%
      mutate(Daily_Test_Positive = c(0,diff(positive))) %>% 
      mutate(Daily_Test_Negative = c(0,diff(negative))) %>% 
      mutate(Daily_Test_All = c(0,diff(total))) %>% 
      mutate(Daily_Hospitalized = c(NA,diff(hospitalized))) %>% 
      mutate(Daily_Deaths = c(NA,diff(death))) %>%
      merge(us_popsize) %>%
      rename(Date = date, Location = state_full, Population_Size = total_pop, Total_Deaths = death, 
             Total_Cases = positive, Total_Hospitalized = hospitalized, 
             Total_Test_Negative = negative, Total_Test_Positive = positive, Total_Test_All = total) %>%
      mutate(Daily_Cases = Daily_Test_Positive, Total_Cases = Total_Test_Positive) %>%
      select(-c(state,Total_Test_Negative,Daily_Test_Negative))
  
    us_ct_clean <- us_clean %>% 
      rename(cases = Daily_Cases,
             hosps = Daily_Hospitalized, 
             deaths = Daily_Deaths) %>%
      dplyr::select(Date, Location, cases, hosps, deaths) 
    
    pseudo_data <- data.frame(
      Date = seq.Date(from = as.Date(start_date), to = max(us_ct_clean$Date), by = "day"),
      hold = NA)
    
    pomp_data <- us_ct_clean %>% 
      group_by(Location) %>% 
      arrange(Date) %>%
      ungroup() %>%
      filter(Location == use_these_locations) %>%
      right_join(pseudo_data, by = "Date") %>%
      mutate(Location = use_these_locations) %>%
      dplyr::select(-hold, -Location) %>%
      mutate(time = 1:n())
    
    return(pomp_data)
}
  

