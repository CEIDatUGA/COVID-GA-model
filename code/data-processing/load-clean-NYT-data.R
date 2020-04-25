# load-clean-NYT-data.R
# load and clean/process NY Times data so it is ready for fitting

# Load libraries ----------------------------------------------------------

 library(dplyr)
 library(readr)
 library(here) #to simplify loading/saving into different folders

#################################
#US data from NY Times
filename_us_nyt_data = here('data',paste0("us-nyt-cleandata-",Sys.Date(),'.rds'))

if (file.exists(filename_us_nyt_data)) {
  #################################
  # load already clean data locally
  #################################
  us_nyt_clean <- readRDS(filename_us_nyt_data)
} else {
  #################################
  # pull data from Covidtracking and process
  #################################
  us_nyt_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  #data for population size for each state/country so we can compute cases per 100K
  #not currently needed, but keep here just in case
  filename = here('data/us_popsize.rds')
  us_popsize <- readRDS(filename)
  us_nyt_clean <- us_nyt_data %>% dplyr::select(c(date,state,cases,deaths)) %>%
    #mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>% 
    group_by(state) %>% arrange(date) %>%
    mutate(Daily_Cases = c(0,diff(cases))) %>% 
    mutate(Daily_Deaths = c(0,diff(deaths))) %>%
    merge(us_popsize) %>%
    rename(Date = date, Location = state, Population_Size = total_pop, Total_Deaths = deaths, 
           Total_Cases = cases)
  
  #this bit of code is to get file names to align with what's currently in the pomp code
  us_nyt_clean <- us_nyt_clean %>% rename(cases = Daily_Cases, deaths = Daily_Deaths)

  saveRDS(us_nyt_clean,filename_us_nyt_data)
}

