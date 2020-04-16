# Daily confirmed deaths #

library(tidyverse) ; library(rtweet) ; library(lubridate) ; library(rvest) ; library(httr) ; library(readxl)

# Public Health England (PHE dashboard)
# https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14

url <- "https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
df <- read_xlsx(tmp, sheet = 3, skip = 6) %>% 
  select(Date, NewDeaths = Deaths, CumDeaths = UK) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# @DHSCgovuk Twitter updates
# https://twitter.com/DHSCgovuk/

tweets <- get_timeline("DHSCgovuk", n = 3200, retryonratelimit = TRUE)

df <- tweets %>%
  filter(str_detect(text, "UPDATE")) %>% 
  mutate(Date = floor_date(as.Date(created_at), unit = "days")) %>%
  select(Date, text) %>%
  mutate(CumDeaths = parse_number(str_extract(text, "(\\d+)\\s+patients*"))) %>%
  filter(!is.na(CumDeaths)) %>%
  arrange(Date) %>% 
  mutate(NewDeaths = CumDeaths - lag(CumDeaths, default = first(CumDeaths))) %>% 
  select(Date, NewDeaths, CumDeaths)

# Department of Health and Social Care and Public Health England 
# https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public#number-of-cases

url <- "https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public#number-of-cases"
parse_number(str_extract(html_text(html_nodes(read_html(url), "p:nth-child(5)"))[1],"(?<=, ).*(?= patients)"))

# Public Health England (DailyIndicators table)
# https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67

url <- "https://www.arcgis.com/sharing/rest/content/items/bc8ee90225644ef7a6f4dd1b13ea1d67/data"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
df <- read_xlsx(tmp) %>% 
  mutate(DateVal = as.Date(DateVal, format = "%d/%m/%y"))

# European Centre for Disease Prevention and Control
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
GET(url, authenticate(":", ":", type = "ntlm"), write_disk(tmp <- tempfile(fileext = ".xlsx")))
df <- read_excel(tmp) %>% 
  filter(countriesAndTerritories == "United_Kingdom") %>% 
  select(dateRep, deaths) %>% 
  arrange(dateRep) %>% 
  mutate(dateRep = as.Date(dateRep, format = "%Y/%b/%d")-1,
          cumDeaths = cumsum(deaths)) %>% 
  filter(dateRep >= "2020-03-05")
