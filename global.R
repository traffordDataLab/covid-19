library(tidyverse) ; library(lubridate) ; library(sf)

# Confirmed cases
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk
cases <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
  filter(`Area type` == "Lower tier local authority") %>%
  mutate(`Specimen date` = as.Date(`Specimen date`, format = "%Y-%m-%d")) %>% 
  select(date = `Specimen date`,
         area_code = `Area code`,
         area_name = `Area name`,
         new_cases = `Daily lab-confirmed cases`) %>% 
  arrange(date) %>% 
  group_by(area_code, area_name) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(new_cases = replace_na(new_cases, 0),
         cum_cases = cumsum(new_cases)) %>% 
  ungroup() %>% 
  fill(area_name)

# Local Authority Districts in England
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc
ltla <- select(st_read("data/ltla.geojson"),-area_name)

# Mid-2018 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100&select=geography_code,obs_value") %>% 
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
  # combine population estimates for Cornwall and Isles of Scilly
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code)) %>% 
  group_by(area_code) %>% 
  summarise(population = sum(population))
