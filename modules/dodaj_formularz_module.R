dodajFormularzModuleServer <- function(id, input, output, session, con) {
  observe({
    kategorie <- dbGetQuery(con, "SELECT id_kategorii, nazwa_kategorii FROM kategorie")
    updateSelectInput(session, "kategoria", 
                      choices = setNames(kategorie$id_kategorii, kategorie$nazwa_kategorii))
  })
  
  observe({
    producenci <- dbGetQuery(con, "SELECT id_producenta, nazwa_producenta FROM producenci")
    updateSelectInput(session, "producent", 
                      choices = setNames(producenci$id_producenta, producenci$nazwa_producenta))
  })
  
  observeEvent(input$dodaj_produkt, {
    nazwa <- input$nazwa_produktu
    kategoria_id <- input$kategoria
    cena_zakupu <- input$cena_zakupu
    cena_sprzedazy <- input$cena_sprzedazy
    kolor <- input$kolor
    producent_id <- input$producent
    kod_towaru <- input$kod_towaru
    opis <- input$opis
    ilosc_magazyn <- input$ilosc_magazyn
    
    data_zamowienia <- input$data_zamowienia 
    
    if (nazwa == "" || is.null(kategoria_id) || is.null(producent_id) || kod_towaru == "") {
      showNotification("Proszę wypełnić wszystkie wymagane pola.", type = "error")
      return(NULL)
    }
    
    query <- "
      INSERT INTO produkty 
      (
        nazwa_produktu, id_kategorii, cena_zakupu, cena_sprzedazy,
        kolor, id_producenta, kod_towaru, opis, ilosc_magazyn, data_zamowienia
      )
      VALUES 
      (
        :nazwa, :kategoria, :zakup, :sprzedaz,
        :kolor, :producent, :kod, :opis, :magazyn, :data_zam
      )
    "
    
    dbExecute(con, query, params = list(
      nazwa = nazwa,
      kategoria = kategoria_id,
      zakup = cena_zakupu,
      sprzedaz = cena_sprzedazy,
      kolor = kolor,
      producent = producent_id,
      kod = kod_towaru,
      opis = opis,
      magazyn = ilosc_magazyn,
      data_zam = as.character(data_zamowienia) 
    ))
    
    showNotification("Produkt dodany pomyślnie!", type = "message")
    
    output$tabelaProduktyView <- renderDT({
      produkty <- dbGetQuery(con, "SELECT id_produktu, nazwa_produktu, cena_sprzedazy, ilosc_magazyn FROM produkty")
      datatable(
        produkty,
        selection = "single",
        options   = list(pageLength = 10, scrollY = "300px", scrollCollapse = TRUE, autoWidth = TRUE)
      )
    })
    
    updateTextInput(session, "nazwa_produktu", value = "")
    updateSelectInput(session, "kategoria", selected = character(0))
    updateNumericInput(session, "cena_zakupu", value = 0)
    updateNumericInput(session, "cena_sprzedazy", value = 0)
    updateTextInput(session, "kolor", value = "")
    updateSelectInput(session, "producent", selected = character(0))
    updateTextInput(session, "kod_towaru", value = "")
    updateTextAreaInput(session, "opis", value = "")
    updateNumericInput(session, "ilosc_magazyn", value = 0)
    updateDateInput(session, "data_zamowienia", value = as.Date("2024-12-27"))
  })
}
