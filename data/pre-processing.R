library(tidyverse) ; library(sf) ; library(readxl) ; library(httr)

# -------------------------------------------
# Geospatial data
# ------------------------------------------- 

# Local Authority Districts in England
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc

# NB dissolve polygons for Cornwall and Isles of Scilly

ltla <- st_read("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson") %>% 
  select(area_code = lad19cd, area_name = lad19nm, long, lat, st_areashape) %>% 
  filter(str_detect(area_code, "^E")) %>% 
  mutate(area_code = as.character(area_code),
         area_code = case_when(area_code %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code),
         area_name = as.character(area_name),
         area_name = case_when(area_code == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ area_name),
         long = case_when(area_code == "E06000052" ~ -4.64249, TRUE ~ long),
         lat = case_when(area_code == "E06000052" ~ 50.45023, TRUE ~ lat)) %>% 
  group_by(area_name, area_code, long, lat) %>% 
  summarise(st_areashape = sum(st_areashape)) %>% 
  select(-st_areashape)

# -------------------------------------------
# Population estimates
# ------------------------------------------- 

# Mid-2019 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

# NB combine population estimates for Cornwall and Isles of Scilly

population <- population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100&select=geography_code,obs_value") %>%  
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code)) %>% 
  group_by(area_code) %>% 
  summarise(population = sum(population))

# ------------------------------------------- 

# write as GeoJSON

left_join(ltla, population, by = "area_code") %>%
  st_write("ltla.geojson")
