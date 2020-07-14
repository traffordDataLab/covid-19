library(tidyverse) ; library(sf)

# Local Authority Districts in England
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc

# NB dissolve polygons for Cornwall and Isles of Scilly

st_read("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson") %>% 
  select(area_code = lad19cd, area_name = lad19nm, long, lat, st_areashape) %>% 
  filter(str_detect(area_code, "^E")) %>% 
  mutate(area_code = as.character(area_code),
         area_name = as.character(area_name),
         area_name = case_when(
           area_name %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly",
           area_name %in% c("City of London", "Hackney") ~ "Hackney and City of London", 
           TRUE ~ area_name),
         area_code = case_when(
           area_name == "Cornwall and Isles of Scilly" ~ "E06000052",
           area_name == "Hackney and City of London" ~ "E09000012",
           TRUE ~ area_code),
         long = case_when(
           area_name == "Cornwall and Isles of Scilly" ~ -4.64249,
           area_name == "Hackney and City of London" ~ -0.06045,
           TRUE ~ long),
         lat = case_when(
           area_name == "Cornwall and Isles of Scilly" ~ 50.45023,
           area_name == "Hackney and City of London" ~ 51.55492,
           TRUE ~ lat)
  ) %>% 
  group_by(area_name, area_code, long, lat) %>% 
  summarise(st_areashape = sum(st_areashape)) %>% 
  select(-st_areashape) %>% 
  st_write("ltla.geojson")
