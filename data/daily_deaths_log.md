
### Daily deaths log

<br/>
**Data sources**

- Updates on the <a href="https://twitter.com/DHSCgovuk" target="_blank">@DHSCgovuk</a> Twitter feed

```
library(tidyverse) ; library(rtweet) ; library(lubridate)

tweets <- get_timeline("DHSCgovuk", n = 3200, retryonratelimit = TRUE)

df <- tweets %>%
  filter(str_detect(text, "UPDATE")) %>% 
  mutate(Date = floor_date(as.Date(created_at), unit = "days")) %>%
  select(Date, text) %>%
  mutate(CumDeaths = parse_number(str_extract(text, ("(\\d+)\\s+patients*") )))%>%
  filter(!is.na(CumDeaths)) %>%
  arrange(Date) %>% 
  mutate(NewDeaths = CumDeaths - lag(CumDeaths, default = first(CumDeaths))) %>% 
  select(Date, NewDeaths, CumDeaths)
```

- <a href="https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases" target="_blank">Public Health England's</a> <a href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14" target="_blank">COVID-19 dashboard</a> and <a href="https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67" target="_blank">DailyIndicators</a> table

```
library(httr) ; library(readxl)
url <- "https://www.arcgis.com/sharing/rest/content/items/bc8ee90225644ef7a6f4dd1b13ea1d67/data"
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
df <- read_xlsx(tmp) %>% 
  mutate(DateVal = as.Date(DateVal, format = "%d/%m/%y"))
```

- <a href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide" target="_blank">European Centre for Disease Prevention and Control's</a> time series data

```
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
GET(url, authenticate(":", ":", type = "ntlm"), write_disk(tmp <- tempfile(fileext = ".xlsx")))

df <- read_excel(tmp) %>% 
  filter(`Countries and territories` == "United_Kingdom") %>% 
  select(DateRep, Deaths) %>% 
  arrange(DateRep) %>% 
  mutate(DateRep = as.Date(DateRep, format = "%Y/%b/%d")-1,
         CumDeaths = cumsum(Deaths)) %>% 
  filter(DateRep >= "2020-03-05")
```

---

**Data source log**

|Date-Time |NewDeaths |CumDeath |Source  | 
|:--- |:--- |:--- |:--- 
|`2020-03-25`| 43 | 465 | <a href="https://news.sky.com/story/coronavirus-uk-death-toll-reaches-435-after-rise-in-scotland-and-wales-11963431" target="_blank">Sky News</a> |
|`2020-03-25 23:03`| 41 | 463 | <a href="https://twitter.com/DHSCgovuk/status/1242950122981470208?s=20" target="_blank">@DHSCgovuk</a> |

