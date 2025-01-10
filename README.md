# magazyn

## Instalacja pakietów

Pakiety można zainstalować poprzez ten skrypt:

```r
packages <- c(
  "shiny", "shinydashboard", "DBI", "RSQLite", "openxlsx", 
  "readr", "dplyr", "DT", "readxl", "lubridate", "ggplot2", 
  "writexl", "shinyjs", "leaflet"
)

install.packages(setdiff(packages, installed.packages()[, "Package"]))
```

## DB

Bazę danych sqlite można zobaczyć wchodząc do katalogu `db` i odpalenie komendy `sqlite3 sklep.db`