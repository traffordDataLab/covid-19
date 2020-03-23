library(tidyverse) ; library(httr) ; library(readxl)

# Latest confirmed cases by country and total UK deaths
# Source: Public Health England
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

# Time series of daily confirmed cases
# Source: Public Health England
# URL: https://www.arcgis.com/home/item.html?id=e5fd11150d274bebaaf8fe2a7a2bda11
url <- "https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
daily_cases <- read_xlsx(tmp) %>% 
  mutate(DateVal = as.Date(DateVal, format = "%d/%m/%y"))

# Confirmed cases by Upper Tier Local Authority (UTLA) in England
# Source: Public Health England
# URL: https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6
utla_cases <- read_csv("https://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data") %>% 
  select(area_code = GSS_CD, TotalCases) %>% 
  bind_rows(cases_by_country)

cases_by_area <- st_read("data/areas.geojson") %>% 
  left_join(utla_cases, by = "area_code") %>%
  mutate(rate = round((TotalCases/population)*100000, 1),
         cases_popup = str_c("<strong>", area_name, "</strong><br/>", TotalCases, " cases") %>% map(HTML),
         rate_popup = str_c("<strong>", area_name, "</strong><br/>", rate, " cases per 100,000 population") %>% map(HTML)) %>% 
  select(-geometry, everything())

# New UK deaths
# Source: European Centre for Disease Prevention and Control
# URL: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
GET(url, authenticate(":", ":", type = "ntlm"), write_disk(tmp <- tempfile(fileext = ".xlsx")))
deaths <- read_excel(tmp) %>% 
  filter(`Countries and territories` == "United_Kingdom") %>% 
  select(DateRep, Deaths) %>% 
  arrange(DateRep) %>% 
  mutate(DateRep = as.Date(DateRep, format = "%Y/%b/%d")-1,
         CumDeaths = cumsum(Deaths)) %>% 
  filter(DateRep >= "2020-01-31")