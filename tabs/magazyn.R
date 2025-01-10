library(shinyjs)

magazyn_tab <- tabItem(
  tabName = "magazyn",
  h2("Magazyn"),
  fluidRow(
    box(
      title = "Produkty w Magazynie",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      DTOutput("tabelaProduktyMagazyn"),
      br(),
      actionButton("przenies_do_sprzedanych", "Przenieś Do Sprzedanych", class = "btn btn-warning"),
      downloadButton("pobierz_excel_magazyn", "Eksportuj do Excel"),
    )
  ),
  fluidRow(
    box(
      title = "Sprzedane Produkty",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      DTOutput("tabelaProduktySprzedane"),
      br(),
      actionButton("zwrot_produktu", "Zwrot", class = "btn btn-danger"),
      downloadButton("pobierz_excel_sprzedane", "Eksportuj do Excel"),
    )
  ),
  fluidRow(
    box(
      title = "Zwroty",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      DTOutput("tabelaZwroty"),
      br(),
      downloadButton("pobierz_excel_zwroty", "Eksportuj do Excel"),
    )
  )
)
