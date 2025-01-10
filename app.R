library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(openxlsx)
library(readr)
library(dplyr)
library(DT)
library(readxl)
library(lubridate)

source("tabs/dashboard_tab.R")
source("tabs/dzialania_tab.R")
source("tabs/dane_podglad_tab.R")
source("tabs/dane_dodaj_tab.R")
source("tabs/dane_modyfikuj_tab.R")
source("tabs/dane_usun_tab.R")
source("tabs/magazyn.R")
source("tabs/porownaj_tab.R")

source("modules/dashboard_module.R")
source("modules/dodaj_formularz_module.R")
source("modules/dodaj_excel_module.R")
source("modules/dodaj_podglad_module.R")
source("modules/usun_module.R")
source("modules/modyfikuj_module.R")
source("modules/magazyn_module.R")
source("modules/rekomendacje_module.R")
source("modules/podglad_module.R")
source("modules/porownaj_module.R")


ui <- dashboardPage(
  dashboardHeader(title = "Media Markt"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Działania", tabName = "dzialania",  icon = icon("tasks")),
      menuItem("Porównaj", tabName = "porownaj",  icon = icon("balance-scale")),
      menuItem("Produkty",    icon = icon("database"),
        menuSubItem("Podgląd", tabName = "podglad"),
        menuSubItem("Dodaj", tabName = "dodaj"),
        menuSubItem("Modyfikuj", tabName = "modyfikuj"),
        menuSubItem("Usuń", tabName = "usun")
      ),
      menuItem("Magazyn", tabName = "magazyn", icon = icon("boxes"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      dashboard_tab,
      dzialania_tab,
      dane_podglad_tab,
      dane_dodaj_tab,
      dane_modyfikuj_tab,
      dane_usun_tab,
      magazyn_tab,
      porownaj_tab
    )
  )
)

server <- function(input, output, session) {
  con <- dbConnect(SQLite(), "db/sklep.db")

  dashboardModuleServer("dashboard1", input, output, session, con)
  dodajFormularzModuleServer("dodajFormularz1", input, output, session, con)
  dodajExcelModuleServer("dodajExcel1", input, output, session, con)
  dodajPodgladModuleServer("dodajPodglad1", input, output, session, con)
  usunModuleServer("usun1", input, output, session, con)
  modyfikujModuleServer("modyfikuj1", input, output, session, con)
  magazynModuleServer("magazyn1", input, output, session, con)
  rekomendacjeModuleServer("rekomendacje1", input, output, session, con)
  podgladModuleServer("podglad1", input, output, session, con)
  porownajModuleServer("porownaj1", input, output, session, con)
  
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
