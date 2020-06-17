library(tidyverse) ; library(sf) ; library(readxl) ; library(httr)

# -------------------------------------------
# Geospatial data
# ------------------------------------------- 

# Local Authority Districts in England
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc

# NB dissolve polygons for Cornwall and Isles of Scilly

lad <- st_read("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson") %>% 
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
# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland

# NB combine population estimates for Cornwall and Isles of Scilly

tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
    write_disk(tmp))

population <- read_xls(tmp, sheet = 6, range = "A5:D420") %>% 
  filter(Geography1 %in% c("London Borough", "Metropolitan District", 
                           "Non-metropolitan District", "Unitary Authority")) %>% 
  select(area_code = Code, population = `All ages`) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code)) %>% 
  group_by(area_code) %>% 
  summarise(population = sum(population))

# ------------------------------------------- 

# write as GeoJSON

left_join(lad, population, by = "area_code") %>% 
  st_write("lad.geojson")
