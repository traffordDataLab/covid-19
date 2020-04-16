library(tidyverse) ; library(httr) ; library(jsonlite)

# -------------------------------------------
# UK cases and deaths
# -------------------------------------------

# Time series of daily confirmed cases in the UK
# Source: Department of Health and Social Care
# URL: https://twitter.com/DHSCgovuk
daily_cases <- read_csv("https://github.com/traffordDataLab/covid-19/raw/master/data/daily_cases.csv", col_types = cols(
  Date = col_date(format = "%Y-%m-%d"),
  NewCases = col_integer(),
  CumCases = col_integer()))

# Time series of daily confirmed deaths in UK hospitals
# Source: Department of Health and Social Care
# URL: https://twitter.com/DHSCgovuk
daily_deaths <- read_csv("https://github.com/traffordDataLab/covid-19/raw/master/data/daily_deaths.csv", col_types = cols(
  Date = col_date(format = "%Y-%m-%d"),
  NewDeaths = col_integer(),
  CumDeaths = col_integer()))

# -------------------------------------------
# Deaths by selected country
# -------------------------------------------

# Time series of deaths by country
# Source: European Centre for Disease Prevention and Control 
# URL: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
global_deaths <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>% 
  select(dateRep, countriesAndTerritories, deaths) %>% 
  filter(countriesAndTerritories %in% c("China", "France", "Italy", "Germany", "Netherlands", 
                                        "South_Korea", "Spain", "Sweden", "United_Kingdom", 
                                        "United_States_of_America")) %>% 
  mutate(dateRep = as.Date(dateRep, format = "%d/%m/%Y"),
         countriesAndTerritories = case_when(
           countriesAndTerritories == "South_Korea" ~ "South Korea",
           countriesAndTerritories == "United_Kingdom" ~ "UK",
           countriesAndTerritories == "United_States_of_America" ~ "USA",
           TRUE ~ countriesAndTerritories)) %>% 
  group_by(countriesAndTerritories) %>% 
  arrange(dateRep) %>%
  mutate(total_deaths = cumsum(deaths)) %>% 
  filter(total_deaths > 99) %>%
  mutate(days = as.integer(dateRep - min(dateRep))) %>% 
  ungroup()

shortened_days <- max(pull(filter(global_deaths, countriesAndTerritories == "Italy"), days))
global_deaths <- filter(global_deaths, days <= shortened_days)

# -------------------------------------------
# Government Response Stringency Index
# -------------------------------------------

# Government Response Stringency Index
# Source: Blavatnik School of Government, Oxford University
# URL: https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker
stringency_index <- fromJSON(paste0("https://covidtrackerapi.bsg.ox.ac.uk/api/stringency/date-range/2020-01-31/", Sys.Date()), flatten = TRUE) %>% 
  pluck("data") %>% 
  enframe() %>%
  unnest(cols = c(value)) %>% 
  unnest_wider(value) %>% 
  filter(country_code %in% c("DEU", "ESP", "ITA", "SWE", "GBR")) %>% 
  select(date_value, country_code, stringency) %>% 
  mutate(date_value = as.Date(date_value, format = "%Y-%m-%d"),
         country = case_when(
           country_code == "DEU" ~ "Germany", country_code == "ESP" ~ "Spain", 
           country_code == "ITA" ~ "Italy", country_code == "SWE" ~ "Sweden", 
           country_code == "GBR" ~ "UK"
         ))
