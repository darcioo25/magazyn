dodajPodgladModuleServer <- function(id, input, output, session, con) {
  output$tabelaProduktyView <- renderDT({
    produkty <- dbGetQuery(con, "SELECT id_produktu, nazwa_produktu, cena_sprzedazy, ilosc_magazyn FROM produkty")
    datatable(
      produkty,
      selection = 'single',
      options   = list(
        pageLength = 10,
        scrollY    = "300px",
        scrollCollapse = TRUE,
        autoWidth  = TRUE
      )
    )
  })
  
  observeEvent(input$tabelaProduktyView_rows_selected, {
    selected_row <- input$tabelaProduktyView_rows_selected
    if (length(selected_row)) {
      selected_data <- dbGetQuery(con, "
        SELECT 
            p.id_produktu, 
            p.nazwa_produktu, 
            p.cena_zakupu, 
            p.cena_sprzedazy, 
            p.kolor, 
            p.id_producenta, 
            p.kod_towaru, 
            p.opis, 
            p.ilosc_magazyn, 
            p.ilosc_sprzedanych, 
            p.ilosc_reklamacji,
            p.data_zamowienia,
            k.nazwa_kategorii   AS nazwa_kategorii,
            pr.nazwa_producenta AS nazwa_producenta
        FROM produkty p
        INNER JOIN kategorie  k  ON p.id_kategorii   = k.id_kategorii
        INNER JOIN producenci pr ON p.id_producenta = pr.id_producenta;
      ")[selected_row, ]

      selected_data$data_zamowienia <- as.Date(
        as.POSIXct(
          selected_data$data_zamowienia,
          origin = "1970-01-01", tz = "UTC"
        )
      )
      
      showModal(modalDialog(
        title     = paste("Szczegóły produktu:", selected_data$nazwa_produktu),
        size      = "l",
        easyClose = TRUE,
        footer    = NULL,
        fluidRow(
          column(6,
                 tags$strong("ID produktu: "), selected_data$id_produktu, br(),
                 tags$strong("Kategoria: "), selected_data$nazwa_kategorii, br(),
                 tags$strong("Cena zakupu: "), selected_data$cena_zakupu, " zł", br(),
                 tags$strong("Cena sprzedaży: "), selected_data$cena_sprzedazy, " zł", br(),
                 tags$strong("Kolor: "), selected_data$kolor, br(),
                 tags$strong("Producent: "), selected_data$nazwa_producenta, br(),
                 tags$strong("Kod towaru: "), selected_data$kod_towaru, br()
          ),
          column(6,
                 tags$strong("Opis: "), selected_data$opis, br(),
                 tags$strong("Ilość na magazynie: "), selected_data$ilosc_magazyn, br(),
                 tags$strong("Ilość sprzedanych: "), selected_data$ilosc_sprzedanych, br(),
                 tags$strong("Ilość reklamacji: "), selected_data$ilosc_reklamacji, br(),
                 tags$strong("Data zamówienia: "), selected_data$data_zamowienia, br()
          )
        )
      ))
    }
  })
}
