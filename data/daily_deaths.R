# New confirmed UK deaths #

library(tidyverse) ; library(httr) ; library(readxl)

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

# Source: @DHSCgovuk Twitter account
# URL: https://twitter.com/DHSCgovuk

library(rtweet) ; library(lubridate)

tweets <- get_timeline("DHSCgovuk", n = 3200, retryonratelimit = TRUE)

deaths <- tweets %>%
  filter(str_detect(text, "UPDATE")) %>% 
  mutate(Date = floor_date(as.Date(created_at), unit = "days")) %>%
  select(Date, text) %>%
  mutate(CumDeaths = parse_number(str_extract(text, ("(\\d+)\\s+patients*") )))%>%
  filter(!is.na(CumDeaths)) %>%
  arrange(Date) %>% 
  mutate(NewDeaths = CumDeaths - lag(CumDeaths, default = first(CumDeaths))) %>% 
  select(Date, NewDeaths, CumDeaths)
