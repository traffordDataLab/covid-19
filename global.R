library(tidyverse) ; library(lubridate) ; library(sf)

# Confirmed cases
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk
cases <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
  filter(`Area type` == "ltla") %>%
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

# Latest 7 days of cases by MSOA
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk/
msoa_cases <- read_csv("https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv") %>% 
  select(lad19cd = lad19_cd,
         lad19nm = lad19_nm,
         msoa11cd = msoa11_cd,
         msoa11hclnm = msoa11_hclnm,
         n = tail(names(.), 1)) %>% 
  mutate(n = ifelse(n == -99, 0, n),
         n = as.integer(n)) %>% 
  relocate(n, .after = last_col()) %>% 
  # combine Hackney and City of London / Cornwall and Isles of Scilly 
  mutate(lad19nm = case_when(
    lad19nm %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly",
    lad19nm %in% c("City of London", "Hackney") ~ "Hackney and City of London", 
    TRUE ~ lad19nm))

# MSOAs in England
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bgc
msoa <- st_read("data/msoa.geojson")

# Mid-2019 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
population <- read_csv("data/population.csv")

