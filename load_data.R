library(tidyverse) ; library(httr) ; library(readxl) ; library(sf) ; library(jsonlite)

# -------------------------------------------
# UK cases and deaths
# -------------------------------------------

# Total confirmed cases for countries of the UK and total deaths in the UK
# Source: Public Health England
# URL: https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67
url <- "https://www.arcgis.com/sharing/rest/content/items/bc8ee90225644ef7a6f4dd1b13ea1d67/data"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
daily_indicators <- read_xlsx(tmp) %>% 
  mutate(DateVal = as.Date(DateVal, format = "%d/%m/%y"))

# Confirmed cases by countries of the UK
cases_by_country <- tibble(
  area_code = c("N92000002", "S92000003", "W92000004"),
  TotalCases = c(daily_indicators$NICases, daily_indicators$ScotlandCases, daily_indicators$WalesCases)
)

# Time series of daily confirmed cases in the UK
# Source: Public Health England
# URL: https://www.arcgis.com/home/item.html?id=e5fd11150d274bebaaf8fe2a7a2bda11
url <- "https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
daily_cases <- read_xlsx(tmp) %>% 
  mutate(DateVal = as.Date(DateVal, format = "%d/%m/%y"),
         CMODateCount = as.integer(CMODateCount),
         CumCases = as.integer(CumCases))

# Total confirmed cases by Upper Tier Local Authority in England
# Source: Public Health England
# URL: https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6
utla_cases <- read_csv("https://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data") %>% 
  select(area_code = GSS_CD, TotalCases) %>% 
  bind_rows(cases_by_country)

cases_by_area <- st_read("data/areas.geojson") %>% 
  left_join(utla_cases, by = "area_code") %>%
  mutate(rate = round((TotalCases/population)*100000, 1),
         cases_popup = str_c("<strong>", area_name, "</strong><br/>", comma(TotalCases), " cases") %>% map(HTML),
         rate_popup = str_c("<strong>", area_name, "</strong><br/>", rate, " cases per 100,000 population") %>% map(HTML)) %>% 
  select(-geometry, everything())

# Time series of daily confirmed deaths in the UK
# Source: Public Health England
# URL: https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx
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