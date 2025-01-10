library(shinydashboard)
library(DT)
library(shinyjs)

dane_podglad_tab <- tabItem(
    tabName = "podglad",
    fluidRow(
        box(
            title = "Podgląd produktów",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            div(
                style = "max-height: 500px; overflow-y: auto;",
                DTOutput("podgladView")
            ),
            br(), br(),
            downloadButton("download_excel", "Eksportuj do Excel"),
        )
    )
)
