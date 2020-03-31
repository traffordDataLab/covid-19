# COVID-19 UK monitor
[R](https://cran.r-project.org) [Shiny](https://cran.r-project.org/web/packages/shiny/index.html) dashboard showing daily confirmed coronavirus cases and deaths in the UK.    

<img src="screenshot.png" width="50%"> 

---

#### Data sources

[Public Health England](https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases) (PHE) publish the following datasets:      

- [Total confirmed cases for countries of the UK and total deaths in the UK](https://www.arcgis.com/home/item.html?id=bc8ee90225644ef7a6f4dd1b13ea1d67)
- [Time series of daily confirmed cases in the UK](https://www.arcgis.com/home/item.html?id=e5fd11150d274bebaaf8fe2a7a2bda11)
- [Total confirmed cases by Upper Tier Local Authority in England](https://www.arcgis.com/home/item.html?id=b684319181f94875a6879bbc833ca3a6)     

A [time series dataset of daily and cumulative confirmed deaths in the UK](https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx) is published on the [PHE dashboard](https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14). Some missing values in the data have been filled using historic updates from the [@DHSCgovuk](https://twitter.com/DHSCgovuk) Twitter feed.
