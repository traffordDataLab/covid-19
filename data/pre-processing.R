library(tidyverse) ; library(sf)

# MSOA names
# Source: House of Commons Library
# URL: https://houseofcommonslibrary.github.io/msoanames/

# Uses the specific versioned URL for reproducibility
msoa_names <- read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv") %>%
    select(msoa21cd, msoa21hclnm)

# Census lookup
# Source: ONS Open Geography Portal
# Source: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2021-lookup-in-england-and-wales-v2-1/about

msoa_lookup <- read_csv("https://www.arcgis.com/sharing/rest/content/items/792f7ab3a99d403ca02cc9ca1cf8af02/data") %>% 
  rename_all(tolower) %>% 
  distinct(msoa21cd, .keep_all = TRUE) %>% 
  left_join(msoa_names, by = "msoa21cd") %>% 
  select(msoa21cd, msoa21hclnm, lad22cd, lad22nm) %>% 
  write_csv("msoa_lookup.csv")

# MSOAs boundaries
# Source: ONS Open Geography Portal
# Generalised resolution
# Source: https://geoportal.statistics.gov.uk/datasets/ons::middle-layer-super-output-areas-2021-boundaries-ew-bgc/about
# Licence: OGL 3.0

msoas <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/MSOA_2021_EW_BGC_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    rename_all(tolower) %>%
    st_as_sf(crs = 4326, coords = c("long", "lat")) %>%
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
    select(msoa21cd, msoa21nm, lon, lat) %>%
    st_write("msoa.geojson")

# Mid-2022 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala -> https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002
# NOMIS selections:
#   - Geography: local authorities: district / unitary (as of April 2021) [All]
#   - Date [2022]
#   - Age [All Ages]
#   - Sex [Total]
# Licence: OGL 3.0

population <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1811939329...1811939332,1811939334...1811939336,1811939338...1811939428,1811939436...1811939442,1811939768,1811939769,1811939443...1811939497,1811939499...1811939501,1811939503,1811939505...1811939507,1811939509...1811939517,1811939519,1811939520,1811939524...1811939570,1811939575...1811939599,1811939601...1811939628,1811939630...1811939634,1811939636...1811939647,1811939649,1811939655...1811939664,1811939667...1811939680,1811939682,1811939683,1811939685,1811939687...1811939704,1811939707,1811939708,1811939710,1811939712...1811939717,1811939719,1811939720,1811939722...1811939730,1811939757...1811939767&date=latest&gender=0&c_age=200&measures=20100") %>% 
    rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
    mutate(area_code = case_when(area_code %in% c("E06000052", "E06000053") ~ "E06000052", # combine population estimates for Cornwall and Isles of Scilly
                                 area_code %in% c("E09000012", "E09000001") ~ "E09000012", # combine population estimates for Hackney and City of London
                                 TRUE ~ area_code)) %>%
    group_by(area_code) %>%
    summarise(population = sum(population)) %>%
    write_csv("population.csv")
