# load-clean-JHU-data.R
# load and clean/process JHU Times data so it is ready for fitting

# Load libraries ----------------------------------------------------------

 library(dplyr)
 library(readr)
 library(tidyr)
 library(here) #to simplify loading/saving into different folders


#################################
#US data from JHU
filename_us_jhu = here('data',paste0("us-jhu-cleandata-",Sys.Date(),'.rds'))

#################################
# load already clean data locally
#################################
if (file.exists(filename_us_jhu)) {
  us_jhu_clean <- readRDS(filename_us_jhu)
} else {
  #################################
  # pull data from JHU github and process
  #################################
  us_jhu_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  us_jhu_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  #data for population size for each state/country so we can compute cases per 100K
  #not currently needed, but keep here just in case
  filename = here('data/us_popsize.rds')
  us_popsize <- readRDS(filename)
  # Clean cases
  us_jhu_cases <- us_jhu_cases %>% filter(iso3 == "USA") %>%
    dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key)) %>%
    rename(Location = Province_State)
  us_jhu_cases <- aggregate(. ~ Location, us_jhu_cases, FUN = sum)
  us_jhu_cases_clean <- gather(us_jhu_cases, Date, Cases, -Location)
  # Clean deaths
  us_jhu_deaths <- us_jhu_deaths %>% filter(iso3 == "USA") %>%
    dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key, -Population)) %>%
    rename(Location = Province_State)
  us_jhu_deaths <- aggregate(. ~ Location, us_jhu_deaths, FUN = sum)
  us_jhu_deaths_clean <- gather(us_jhu_deaths, Date, Deaths, -Location)
  us_jhu_combined <- merge(us_jhu_cases_clean, us_jhu_deaths_clean)
  us_jhu_popsize <- us_popsize %>% rename(Location = state)
  # This merge removes cruise ship cases/death counts
  us_jhu_merge <- merge(us_jhu_combined, us_jhu_popsize)
  us_jhu_clean <- us_jhu_merge %>% mutate(Date = as.Date(as.character(Date),format="%m/%d/%y")) %>%
    group_by(Location) %>% arrange(Date) %>%
    mutate(Daily_Cases = c(0,diff(Cases))) %>%
    mutate(Daily_Deaths = c(0,diff(Deaths))) %>% 
    ungroup() %>%
    rename(Total_Deaths = Deaths, Total_Cases = Cases, Population_Size = total_pop) %>% 
    data.frame()
  
  #this bit of code is to get file names to align with what's currently in the pomp code
  us_jhu_clean <- us_jhu_clean %>% rename(cases = Daily_Cases, deaths = Daily_Deaths)
  
  saveRDS(us_jhu_clean,filename_us_jhu)
  
}

