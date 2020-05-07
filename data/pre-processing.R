library(tidyverse) ; library(sf) ; library(httr) ; library(readxl) ; library(janitor) 

# -------------------------------------------
# Deaths by LAD
# ------------------------------------------- 

# Local Authority Districts in England and Wales
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc

lad <- st_read("https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.geojson") %>% 
  select(area_code = lad19cd, area_name = lad19nm) %>% 
  filter(str_detect(area_code, paste(c("^E", "^W"), collapse = '|')))

# Age-standardised COVID-19 mortality rates
# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsinvolvingcovid19bylocalareaanddeprivation

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1march2020to17april2020/referencetablesdraft.xlsx",
    write_disk(tmp))

deaths <- read_xlsx(tmp, sheet = 5, skip = 3) %>%
  clean_names() %>%
  filter(sex == "Persons") %>%
  select(area_code, deaths = covid_19, rate = x12, LCI = x14, UCI = x15) %>% 
  mutate_at(c("deaths","rate","LCI","UCI"),as.numeric)

lad %>% 
  left_join(deaths, by = "area_code") %>% 
  st_write("age_standardised_deaths.geojson")

# -------------------------------------------
# Population estimates
# ------------------------------------------- 

# Mid-2018 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002

population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=2092957702,2092957701,2092957700,1816133633...1816133848&date=latest&gender=0&c_age=200&measures=20100&select=geography_code,obs_value") %>% 
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE)

# -------------------------------------------
# Geospatial data
# ------------------------------------------- 

# Counties and Unitary Authorities in England
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-april-2019-boundaries-ew-buc

st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Counties_and_Unitary_Authorities_December_2019_Boundaries_UK_BUC2/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson") %>% 
  filter(str_detect(ctyua19cd, "^E")) %>% 
  select(area_code = ctyua19cd, area_name = ctyua19nm, long, lat) %>% 
  mutate(area_code = as.character(area_code),
         area_name = as.character(area_name),
         area_name = case_when(area_name %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly", TRUE ~ area_name),
         area_code = case_when(area_name == "Cornwall and Isles of Scilly" ~ "E06000052", TRUE ~ area_code),
         long = case_when(area_name == "Cornwall and Isles of Scilly" ~ -4.64249, TRUE ~ long),
         lat = case_when(area_name == "Cornwall and Isles of Scilly" ~ 50.45023, TRUE ~ lat)) %>% 
  left_join(population, by = "area_code") %>% 
  group_by(area_name, area_code, long, lat) %>%
  summarise(population = sum(population)) %>% 
  st_write("utla.geojson")


