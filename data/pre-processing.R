# Vector boundaries for countries and UTLAs in England with mid-2018 population estimates #

library(tidyverse) ; library(sf)

# Northern Ireland, Scotland, and Wales
# Source: Open Geography Portal, ONS
# URL: https://geoportal.statistics.gov.uk/datasets/countries-december-2018-boundaries-uk-bgc
countries <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Countries_December_2018_Boundaries_UK_BUC/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>% 
  filter(!str_detect(ctry18cd, "^E")) %>% 
  select(area_code = ctry18cd, area_name = ctry18nm, long, lat)

# Counties and Unitary Authorities in England
# Source: Open Geography Portal, ONS
# URL: https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-april-2019-boundaries-ew-buc
utla <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Counties_and_Unitary_Authorities_April_2019_Boundaries_EW_BUC/MapServer/0/query?where=UPPER(ctyua19cd)%20like%20'%25E_%25'&outFields=*&outSR=4326&f=geojson") %>% 
  select(area_code = ctyua19cd, area_name = ctyua19nm, long, lat)

# Mid-2018 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002
population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=2092957702,2092957701,2092957700,1816133633...1816133848&date=latest&gender=0&c_age=200&measures=20100&select=geography_code,obs_value") %>% 
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE)

# combine areas, join population data and dissolve specific geometries
areas <- rbind(countries, utla) %>% # combine UTLA and country polygons
  left_join(population, by = "area_code") %>% # join population estimates
  mutate(area_name = as.character(area_name),
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
  summarise(population = sum(population)) # dissolve geometries of selected UTLAs

# write results
st_write(areas, "areas.geojson")
