# Vector boundaries for countries and UTLAs in England #

library(tidyverse) ; library(sf)

# Northern Ireland, Scotland, Wales
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

rbind(countries, utla) %>%
  st_write("areas.geojson")
