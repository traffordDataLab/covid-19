# Data published by Public Health England 
# https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases

# Total confirmed cases for countries of the UK and total deaths in the UK
# URL: https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67
url <- "https://www.arcgis.com/sharing/rest/content/items/bc8ee90225644ef7a6f4dd1b13ea1d67/data"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
daily_indicators <- read_xlsx(tmp) %>% 
  mutate(DateVal = as.Date(DateVal, format = "%d/%m/%y"))

# Confirmed cases by country
cases_by_country <- tibble(
  area_code = c("N92000002", "S92000003", "W92000004"),
  TotalCases = c(daily_indicators$NICases, daily_indicators$ScotlandCases, daily_indicators$WalesCases)
)

# Time series of daily confirmed cases in the UK
# URL: https://www.arcgis.com/home/item.html?id=e5fd11150d274bebaaf8fe2a7a2bda11
url <- "https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
daily_cases <- read_xlsx(tmp) %>% 
  mutate(DateVal = as.Date(DateVal, format = "%d/%m/%y"),
         CMODateCount = as.integer(CMODateCount),
         CumCases = as.integer(CumCases))

# Total confirmed cases by Upper Tier Local Authority in England
# URL: https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6
utla_cases <- read_csv("https://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data") %>% 
  select(area_code = GSS_CD, TotalCases) %>% 
  bind_rows(cases_by_country)

cases_by_area <- st_read("data/areas.geojson") %>% 
  left_join(utla_cases, by = "area_code") %>%
  mutate(rate = round((TotalCases/population)*100000, 1),
         cases_popup = str_c("<strong>", area_name, "</strong><br/>", comma(TotalCases), " cases") %>% map(HTML),
         rate_popup = str_c("<strong>", area_name, "</strong><br/>", rate, " cases per 100,000 population") %>% map(HTML)) %>% 
  select(-geometry, everything())

# Time series of daily confirmed deaths in the UK
# URL: https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx
daily_deaths <- read_csv("https://github.com/traffordDataLab/covid-19/raw/master/data/daily_deaths.csv", col_types = cols(
  Date = col_date(format = "%Y-%m-%d"),
  NewDeaths = col_integer(),
  CumDeaths = col_integer()))