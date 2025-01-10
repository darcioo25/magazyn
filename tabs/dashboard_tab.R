library(leaflet)

dashboard_tab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    box(
      title = "Wykres Sprzedaży",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      selectInput(
        inputId = "aggregation",
        label = "Wybierz agregację:",
        choices = c("Tygodnie" = "week", "Miesiące" = "month", "Lata" = "year"),
        selected = "month"
      ),
      plotOutput("sales_plot")
    )
  ),
  fluidRow(
    box(
      title = "Ilość sprzedanych produktów",
      status = "success",
      solidHeader = TRUE,
      width = 6,
      uiOutput("quantity")
    ),
    box(
      title = "Ilość produktów na magazynie",
      status = "success",
      solidHeader = TRUE,
      width = 6,
      uiOutput("warehouse_quantity"),
    )
  ),
  fluidRow(
    box(
      title = "Najlepiej Sprzedający się Produkt",
      status = "success",
      solidHeader = TRUE,
      width = 6,
      uiOutput("best_seller")
    ),
    box(
      title = "Najgorzej Sprzedający się Produkt",
      status = "danger",
      solidHeader = TRUE,
      width = 6,
      uiOutput("worst_seller")
    )
  ),
  fluidRow(
    box(
      title = "Sprzedaż na Producentów",
      status = "info",
      solidHeader = TRUE,
      width = 6,
      plotOutput("sales_producer_plot")
    ),
    box(
      title = "Sprzedaż na Kategorie",
      status = "warning",
      solidHeader = TRUE,
      width = 6,
      plotOutput("sales_category_plot")
    )
  ),
  fluidRow(
    box(
      title = "Sprzedaż na Miejsca",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      plotOutput("sales_location_plot")
    )
  ),
  fluidRow(
    box(
      title = "Mapa Punktów Sprzedaży",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      leafletOutput("sales_map", height = 500)
    )
  )
)
