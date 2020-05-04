library(tidyverse) 

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
# URL: https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker

stringency_index <- read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv") %>%  
  filter(CountryCode %in% c("DEU", "ESP", "ITA", "SWE", "GBR")) %>% 
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"),
         Country = case_when(
           CountryCode == "DEU" ~ "Germany", 
           CountryCode == "ESP" ~ "Spain", 
           CountryCode == "ITA" ~ "Italy", 
           CountryCode == "SWE" ~ "Sweden", 
           CountryCode == "GBR" ~ "UK")) %>% 
  select(Date, Country, Stringency = StringencyIndexForDisplay)
  
