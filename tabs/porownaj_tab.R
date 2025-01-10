library(shiny)
library(shinydashboard)

porownaj_tab <- tabItem(
  tabName = "porownaj",
  
  fluidRow(
    column(12, h2("Porównaj produkty"))
  ),
  
  fluidRow(
    column(
      width = 4,
      box(
        title = "Ustawienia",
        width = 12,
        
        selectInput(
          inputId = "porownaj_kategoria",
          label = "Wybierz kategorię:",
          choices = NULL,  
          selectize = TRUE
        ),
        uiOutput("porownaj_produkty_ui"),
        
        br(),
        uiOutput("porownaj_bestworst")
      )
    ),
    
    column(
      width = 8,
      box(
        title = "Sprzedaż w wybranej kategorii",
        width = 12,
        plotOutput("porownaj_wykres", height = "400px")
      )
    )
  ),
  fluidRow(
    column(
        width = 6,
        uiOutput("box_prod1")
    ),
    column(
        width = 6,
        uiOutput("box_prod2")
    )
   )

)
