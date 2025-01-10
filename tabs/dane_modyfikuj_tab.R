dane_modyfikuj_tab <- tabItem(
  tabName = "modyfikuj",
  fluidRow(
    box(
      title = "Modyfikuj produkty",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      DTOutput("tabelaProduktyDoModyfikacji"),
      br(),
      actionButton("modyfikuj_produkt", "Modyfikuj Wybrany Produkt", class = "btn btn-primary")
    )
  )
)
