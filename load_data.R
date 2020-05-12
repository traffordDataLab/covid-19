library(tidyverse) ; library(readxl) ; library(httr) ; library(sf)

# -------------------------------------------
# UK cases and deaths
# -------------------------------------------

# Source: European Centre for Disease Prevention and Control
# URL: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx",
    write_disk(tmp))
ecdc <- read_xlsx(tmp, sheet = 1)

# ecdc <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
#                  col_types = cols(
#                    dateRep = col_date(format = "%d/%m/%Y"),
#                    day = col_integer(),
#                    month = col_integer(),
#                    year = col_integer(),
#                    cases = col_integer(),
#                    deaths = col_integer(),
#                    countriesAndTerritories = col_character(),
#                    geoId = col_character(),
#                    countryterritoryCode = col_character(),
#                    popData2018 = col_integer()))

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
  filter(countriesAndTerritories %in% c("France", "Italy", "Germany", "Netherlands", 
                                        "Spain", "Sweden", "United_Kingdom")) %>% 
  mutate(dateRep = as.Date(dateRep, format = "%d/%m/%Y"),
         countriesAndTerritories = case_when(
           countriesAndTerritories == "United_Kingdom" ~ "UK",
           TRUE ~ countriesAndTerritories)) %>% 
  group_by(countriesAndTerritories) %>% 
  arrange(dateRep) %>%
  mutate(total_deaths = cumsum(deaths)) %>% 
  filter(total_deaths > 99) %>%
  mutate(days = as.integer(dateRep - min(dateRep))) %>% 
  ungroup()

# -------------------------------------------
# Cases by Local authority District
# -------------------------------------------

# Daily cases
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk/#local-authorities

phe <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
  filter(`Area type` == "Lower tier local authority", `Specimen date` == max(`Specimen date`)) %>% 
  select(area_code = `Area code`, `Specimen date`, cumulative_lab_confirmed_cases = `Cumulative lab-confirmed cases`) 

la_data <- st_read("data/lad.geojson") %>% 
  left_join(phe, by = "area_code") %>% 
  filter(!is.na(long)) %>% 
  select(area_code, area_name, everything()) 

# -------------------------------------------
# Government Response Stringency Index
# -------------------------------------------

# Source: Blavatnik School of Government, Oxford University
# URL: https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker

stringency_index <- read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv") %>%  
  filter(CountryCode %in% c("DEU", "ESP", "FRA", "ITA", "NLD", "SWE", "GBR")) %>% 
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"),
         Country = case_when(
           CountryCode == "DEU" ~ "Germany", 
           CountryCode == "ESP" ~ "Spain", 
           CountryCode == "FRA" ~ "France", 
           CountryCode == "ITA" ~ "Italy", 
           CountryCode == "NLD" ~ "Netherlands", 
           CountryCode == "SWE" ~ "Sweden", 
           CountryCode == "GBR" ~ "UK")) %>% 
  select(Date, Country, Stringency = StringencyIndexForDisplay)
  
