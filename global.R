library(tidyverse) ; library(lubridate) ; library(sf)

# Confirmed cases
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk
#cases <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") 

  
cases <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesByPublishDate&metric=newCasesBySpecimenDate&format=csv") %>%
  filter(`areaType` == "ltla") %>%
  mutate(`date` = as.Date(`date`, format = "%Y-%m-%d")) %>% 
  select(date,
         area_code = `areaCode`,
         area_name = `areaName`,
         new_cases = `newCasesBySpecimenDate`) %>% 
  arrange(date) %>% 
  group_by(area_code, area_name) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(new_cases = replace_na(new_cases, 0),
         cum_cases = cumsum(new_cases)) %>% 
  ungroup() %>% 
  fill(area_name)


# MSOA lookup
# Source: ONS Open Geography Portal; House of Commons Library
msoa_lookup <- read_csv("data/msoa_lookup.csv")

# MSOAs in England
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bgc
msoa <- st_read("data/msoa.geojson") %>%
  left_join(msoa_lookup, by = "msoa11cd") %>% 
  # combine Hackney and City of London / Cornwall and Isles of Scilly 
  mutate(lad19nm = as.character(lad19nm),
         lad19nm = case_when(
           lad19nm %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly",
           lad19nm %in% c("City of London", "Hackney") ~ "Hackney and City of London", 
           TRUE ~ lad19nm))

# Latest 7 days of cases by MSOA
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk/details/download
msoa_cases <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv") %>%
  filter(date == max(date)) %>% 
  select(msoa11cd = areaCode, date, n = newCasesBySpecimenDateRollingSum, rate = newCasesBySpecimenDateRollingRate) %>% 
  mutate(n = replace_na(n, 0),
         rate = replace_na(rate, 0)) 


# Mid-2019 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
population <- read_csv("data/population.csv")

