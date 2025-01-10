dane_usun_tab <- tabItem(
  tabName = "usun",
  fluidRow(
    box(
      title = "Usuń produkty",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      DTOutput("tabelaProduktyUsun"),
      br(),
      actionButton("usun_wybrane", "Usuń Wybrane Produkty", class = "btn btn-danger")
    )
  )
)
