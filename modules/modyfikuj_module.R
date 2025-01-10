modyfikujModuleServer <- function(id, input, output, session, con) {
  selected_product_id <- reactiveVal(NULL)
  
  output$tabelaProduktyDoModyfikacji <- renderDT({
    produkty <- dbGetQuery(con, "SELECT id_produktu, nazwa_produktu, cena_sprzedazy, ilosc_magazyn FROM produkty")
    
    datatable(
      produkty,
      selection = 'single', 
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  observeEvent(input$modyfikuj_produkt, {
    selected_row <- input$tabelaProduktyDoModyfikacji_rows_selected
    
    if (length(selected_row) == 0) {
      showNotification("Proszę wybrać produkt do modyfikacji.", type = "warning")
      return(NULL)
    }
    
    selected_data <- dbGetQuery(con, "
      SELECT 
        id_produktu, 
        nazwa_produktu, 
        id_kategorii, 
        cena_zakupu, 
        cena_sprzedazy, 
        kolor, 
        id_producenta, 
        kod_towaru, 
        opis, 
        ilosc_magazyn,
        data_zamowienia
      FROM produkty
    ")[selected_row, ]
    
    if (nrow(selected_data) == 0) {
      showNotification("Wybrany produkt nie istnieje.", type = "error")
      return(NULL)
    }
    
    selected_product_id(selected_data$id_produktu)
    
    nazwa_kategorii <- dbGetQuery(
      con, 
      "SELECT nazwa_kategorii FROM kategorie WHERE id_kategorii = ?", 
      selected_data$id_kategorii
    )$nazwa_kategorii
    
    nazwa_producenta <- dbGetQuery(
      con, 
      "SELECT nazwa_producenta FROM producenci WHERE id_producenta = ?", 
      selected_data$id_producenta
    )$nazwa_producenta
    
    kategorie <- dbGetQuery(con, "SELECT id_kategorii, nazwa_kategorii FROM kategorie")
    producenci <- dbGetQuery(con, "SELECT id_producenta, nazwa_producenta FROM producenci")
    
    current_date <- tryCatch(
      as.Date(selected_data$data_zamowienia),
      error = function(e) Sys.Date()
    )
    
    showModal(modalDialog(
      title = paste("Modyfikuj produkt:", selected_data$nazwa_produktu),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Anuluj"),
        actionButton("zapisz_modyfikacje", "Zapisz Zmiany", class = "btn btn-success")
      ),
      fluidRow(
        column(6,
               textInput("m_nazwa_produktu", "Nazwa produktu", value = selected_data$nazwa_produktu),
               selectInput("m_kategoria", "Kategoria", 
                           choices = setNames(kategorie$id_kategorii, kategorie$nazwa_kategorii),
                           selected = selected_data$id_kategorii),
               numericInput("m_cena_zakupu", "Cena zakupu (zł)", 
                            value = selected_data$cena_zakupu, min = 0),
               numericInput("m_cena_sprzedazy", "Cena sprzedaży (zł)", 
                            value = selected_data$cena_sprzedazy, min = 0),
               textInput("m_kolor", "Kolor", value = selected_data$kolor)
        ),
        column(6,
               selectInput("m_producent", "Producent", 
                           choices = setNames(producenci$id_producenta, producenci$nazwa_producenta),
                           selected = selected_data$id_producenta),
               textInput("m_kod_towaru", "Kod towaru (SKU)", value = selected_data$kod_towaru),
               textAreaInput("m_opis", "Opis", value = selected_data$opis, rows = 3),
               numericInput("m_ilosc_magazyn", "Ilość na magazynie", 
                            value = selected_data$ilosc_magazyn, min = 0),
               
               dateInput(
                 "m_data_zamowienia", 
                 label = "Data zamówienia (YYYY-MM-DD)", 
                 value = current_date,
                 format = "yyyy-mm-dd"
               )
        )
      )
    ))
  })
  
  observeEvent(input$zapisz_modyfikacje, {
    nazwa_produktu    <- input$m_nazwa_produktu
    id_kategorii      <- input$m_kategoria
    cena_zakupu       <- input$m_cena_zakupu
    cena_sprzedazy    <- input$m_cena_sprzedazy
    kolor             <- input$m_kolor
    id_producenta     <- input$m_producent
    kod_towaru        <- input$m_kod_towaru
    opis              <- input$m_opis
    ilosc_magazyn     <- input$m_ilosc_magazyn
    
    data_zamowienia   <- format(input$m_data_zamowienia, "%Y-%m-%d")
    
    if (nazwa_produktu == "" || is.null(id_kategorii) || 
        is.null(id_producenta) || kod_towaru == "" || data_zamowienia == "") {
      showNotification("Proszę wypełnić wszystkie wymagane pola.", type = "error")
      return(NULL)
    }
    
    selected_id <- selected_product_id()
    print(paste("Wybrane id_produktu do modyfikacji:", selected_id))
    
    if (is.null(selected_id) || length(selected_id) == 0) {
      showNotification("Nie można znaleźć wybranego produktu do modyfikacji.", type = "error")
      return(NULL)
    }
    
    query <- "
      UPDATE produkty 
      SET 
        nazwa_produktu = :nazwa_produktu,
        id_kategorii = :id_kategorii,
        cena_zakupu = :cena_zakupu,
        cena_sprzedazy = :cena_sprzedazy,
        kolor = :kolor,
        id_producenta = :id_producenta,
        kod_towaru = :kod_towaru,
        opis = :opis,
        ilosc_magazyn = :ilosc_magazyn,
        data_zamowienia = :data_zamowienia
      WHERE id_produktu = :id_produktu
    "
    
    result <- tryCatch({
      dbExecute(con, query, params = list(
        nazwa_produktu = nazwa_produktu,
        id_kategorii = id_kategorii,
        cena_zakupu = cena_zakupu,
        cena_sprzedazy = cena_sprzedazy,
        kolor = kolor,
        id_producenta = id_producenta,
        kod_towaru = kod_towaru,
        opis = opis,
        ilosc_magazyn = ilosc_magazyn,
        data_zamowienia = data_zamowienia,
        id_produktu = selected_id
      ))
    }, error = function(e) {
      showNotification(paste("Błąd podczas aktualizacji produktu:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(result)) {
      showNotification("Produkt zmodyfikowany pomyślnie!", type = "message")
      
      removeModal()
      selected_product_id(NULL)
      
      output$tabelaProduktyDoModyfikacji <- renderDT({
        produkty <- dbGetQuery(con, "SELECT id_produktu, nazwa_produktu, cena_sprzedazy, ilosc_magazyn FROM produkty")
        
        datatable(
          produkty,
          selection = 'single',  
          options = list(
            pageLength = 10,
            scrollY = "300px",
            scrollCollapse = TRUE,
            autoWidth = TRUE
          )
        )
      })
      
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
    }
  })
}