# COVID-19 monitor
[R](https://cran.r-project.org) [Shiny](https://cran.r-project.org/web/packages/shiny/index.html) dashboard showing daily confirmed coronavirus cases and deaths in the UK.    

<img src="screenshot.png" width="50%"> 

---

#### Data sources

[Public Health England](https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases) (PHE) publish the following datasets:      

- [Total confirmed cases for countries of the UK and total deaths in the UK](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67)
- [Time series of daily confirmed cases in the UK](https://www.arcgis.com/home/item.html?id=e5fd11150d274bebaaf8fe2a7a2bda11)
- [Total confirmed cases by Upper Tier Local Authority in England](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6)     

A [time series dataset of daily confirmed deaths in the UK](data/daily_deaths.csv) is manually collated from the [@DHSCgovuk](https://twitter.com/DHSCgovuk) Twitter feed and the PHE [DailyIndicators](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67) table.
