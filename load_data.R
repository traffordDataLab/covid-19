library(tidyverse) ; library(jsonlite)

# -------------------------------------------
# Mid-2018 population estimates
# -------------------------------------------

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002

# read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1816133652...1816133661&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>%
#   select(AreaCode = GEOGRAPHY_CODE, Persons = OBS_VALUE) %>%
#   write_csv("data/mid-2018_population_estimates.csv")

population <- read_csv("data/mid-2018_population_estimates.csv",
                       col_types = cols(
                         AreaCode = col_character(),
                         Persons = col_integer()))

# -------------------------------------------
# Greater Manchester cases
# -------------------------------------------

# Source: Public Health England
# URL: https://coronavirus.data.gov.uk

gm_data <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv",
                          col_types = cols(
                            `Area name` = col_character(),
                            `Area code` = col_character(),
                            `Area type` = col_character(),
                            `Specimen date` = col_date(format = "%Y-%m-%d"),
                            `Daily lab-confirmed cases` = col_integer(),
                            `Cumulative lab-confirmed cases` = col_integer())) %>% 
  select(Date = `Specimen date`,
         AreaName = `Area name`,
         AreaCode = `Area code`,
         AreaType = `Area type`,
         NewCases = `Daily lab-confirmed cases`,
         CumCases = `Cumulative lab-confirmed cases`) %>% 
  filter(AreaName %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>%
  arrange(Date) %>% 
  left_join(population, by = "AreaCode") %>% 
  mutate(Rate = round((CumCases/Persons*10000),1))

# -------------------------------------------
# UK cases and deaths
# -------------------------------------------

# Source: European Centre for Disease Prevention and Control
# URL: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

ecdc <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                col_types = cols(
                  dateRep = col_date(format = "%d/%m/%Y"),
                  day = col_integer(),
                  month = col_integer(),
                  year = col_integer(),
                  cases = col_integer(),
                  deaths = col_integer(),
                  countriesAndTerritories = col_character(),
                  geoId = col_character(),
                  countryterritoryCode = col_character(),
                  popData2018 = col_integer()))

uk_data <- ecdc %>% 
  filter(countriesAndTerritories == "United_Kingdom", 
         dateRep >= "2020-01-31") %>% 
  select(Date = dateRep, NewCases = cases, NewDeaths = deaths) %>% 
  arrange(Date) %>% 
  mutate(Date = as.Date(Date, format = "%Y/%b/%d")-1,
         NewCases = as.integer(NewCases),
         NewDeaths = as.integer(NewDeaths),
         CumCases = cumsum(NewCases),
         CumDeaths = cumsum(NewDeaths)) %>% 
  select(Date, NewCases, CumCases, NewDeaths, CumDeaths)

# -------------------------------------------
# Deaths by selected country
# -------------------------------------------

# Source: European Centre for Disease Prevention and Control
# URL: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

country_data <- ecdc %>% 
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

shortened_days <- max(pull(filter(country_data, countriesAndTerritories == "Italy"), days))
country_data <- filter(country_data, days <= shortened_days)

# -------------------------------------------
# Government Response Stringency Index
# -------------------------------------------

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
