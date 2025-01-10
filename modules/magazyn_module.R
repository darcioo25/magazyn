library(writexl)

magazynModuleServer <- function(id, input, output, session, con) {
  output$tabelaProduktyMagazyn <- renderDT({
    produkty_magazyn <- dbGetQuery(con, "
      SELECT 
        p.id_produktu, 
        p.nazwa_produktu, 
        p.cena_sprzedazy, 
        p.ilosc_magazyn, 
        p.kolor, 
        k.nazwa_kategorii, 
        pr.nazwa_producenta
      FROM produkty p
      JOIN kategorie k ON p.id_kategorii = k.id_kategorii
      JOIN producenci pr ON p.id_producenta = pr.id_producenta
      WHERE p.ilosc_magazyn > 0
    ")
    
    datatable(
      produkty_magazyn,
      selection = 'single',  
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })

  output$pobierz_excel_magazyn <- downloadHandler(
    filename = function() {
      paste("produkty-", Sys.Date(), ".xlsx", sep = "")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      produkty_all <- dbGetQuery(con, "SELECT * FROM produkty")
      write_xlsx(produkty_all, path = file)
    }
  )
  
  output$pobierz_excel_sprzedane <- downloadHandler(
    filename = function() {
      paste("produkty-", Sys.Date(), ".xlsx", sep = "")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      produkty_all <- dbGetQuery(con, "SELECT * FROM zakupy")
      write_xlsx(produkty_all, path = file)
    }
  )

  output$pobierz_excel_zwroty <- downloadHandler(
    filename = function() {
      paste("produkty-", Sys.Date(), ".xlsx", sep = "")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      produkty_all <- dbGetQuery(con, "SELECT * FROM zwroty")
      write_xlsx(produkty_all, path = file)
    }
  )

  output$tabelaProduktySprzedane <- renderDT({
    sprzedane_produkty <- dbGetQuery(con, "
      SELECT 
        z.id_zakupu,
        p.id_produktu,
        p.nazwa_produktu,
        z.ilosc_zakupu,
        z.data_zakupu,
        z.kwota_zakupu,
        z.miejsce_zakupu
      FROM zakupy z
      JOIN produkty p ON z.id_produktu = p.id_produktu
    ")
    
    datatable(
      sprzedane_produkty,
      selection = 'single',  
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  output$tabelaZwroty <- renderDT({
    zwroty <- dbGetQuery(con, "
      SELECT 
        z.id_zwrotu,
        p.id_produktu,
        p.nazwa_produktu,
        z.wadliwy,
        z.opis
      FROM zwroty z
      JOIN produkty p ON z.id_produktu = p.id_produktu
    ")
    
    zwroty$wadliwy <- ifelse(zwroty$wadliwy == 1, "Tak", "Nie")
    
    datatable(
      zwroty,
      selection = 'single', 
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  miejsca_sprzedazy <- c("Poznań", "Warszawa", "Gdańsk", "Kraków", "Internet")
  
  selected_product_to_transfer <- reactiveVal(NULL)
  
  observeEvent(input$przenies_do_sprzedanych, {
    selected_row <- input$tabelaProduktyMagazyn_rows_selected
    
    if (length(selected_row) == 0) {
      showNotification("Proszę wybrać produkt do przeniesienia.", type = "warning")
      return(NULL)
    }
    
    produkty_magazyn <- dbGetQuery(con, "
      SELECT 
        p.id_produktu, 
        p.nazwa_produktu, 
        p.cena_sprzedazy, 
        p.ilosc_magazyn, 
        p.kolor, 
        k.nazwa_kategorii, 
        pr.nazwa_producenta
      FROM produkty p
      JOIN kategorie k ON p.id_kategorii = k.id_kategorii
      JOIN producenci pr ON p.id_producenta = pr.id_producenta
      WHERE p.ilosc_magazyn > 0
    ")
    
    selected_product <- produkty_magazyn[selected_row, ]
    
    print("Wybrany produkt do przeniesienia:")
    print(selected_product)
    
    selected_product_to_transfer(list(
      id_produktu = selected_product$id_produktu,
      nazwa_produktu = selected_product$nazwa_produktu,
      ilosc_magazyn = selected_product$ilosc_magazyn
    ))
    
    showModal(modalDialog(
      title = paste("Przenieś produkt:", selected_product$nazwa_produktu),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Anuluj"),
        actionButton("confirm_przeniesienie", "Przenieś", class = "btn btn-success")
      ),
      fluidRow(
        column(12,
               tags$strong("Nazwa produktu: "), selected_product$nazwa_produktu, br(),
               tags$strong("Kategoria: "), selected_product$nazwa_kategorii, br(),
               tags$strong("Producent: "), selected_product$nazwa_producenta, br(),
               tags$strong("Cena Sprzedaży: "), selected_product$cena_sprzedazy, " zł", br(),
               tags$strong("Kolor: "), selected_product$kolor, br(),
               tags$strong("Ilość w magazynie: "), selected_product$ilosc_magazyn, br(),
               selectInput("miejsce_sprzedazy", "Miejsce Sprzedaży:", choices = miejsca_sprzedazy),
               numericInput("ilosc_przeniesienia", "Ilość do przeniesienia:", 
                            min = 1, max = selected_product$ilosc_magazyn, value = 1),
               dateInput("data_sprzedazy", "Data Sprzedaży:", value = Sys.Date(), 
                     max = Sys.Date())
        )
      )
    ))
  })
  
  observeEvent(input$confirm_przeniesienie, {
    req(input$ilosc_przeniesienia)
    req(input$miejsce_sprzedazy)
    req(input$data_sprzedazy)

    selected_product_info <- selected_product_to_transfer()
    
    print("Informacje o produkcie do przeniesienia:")
    print(selected_product_info)
    
    if (is.null(selected_product_info)) {
      showNotification("Nie można znaleźć wybranego produktu.", type = "error")
      removeModal()
      return(NULL)
    }
    
    ilosc_do_przeniesienia <- input$ilosc_przeniesienia
    ilosc_dostepna <- selected_product_info$ilosc_magazyn
    
    if (ilosc_do_przeniesienia > ilosc_dostepna) {
      showNotification("Ilość do przeniesienia przekracza dostępną ilość w magazynie.", type = "error")
      return(NULL)
    }
    
    miejsce_sprzedazy <- input$miejsce_sprzedazy
    
    data_zakupu <- format(input$data_sprzedazy, "%Y-%m-%d")
  
    if (as.Date(data_zakupu) > Sys.Date()) {
        showNotification("Data sprzedaży nie może być w przyszłości.", type = "error")
        return(NULL)
    }

    cena_sprzedazy <- dbGetQuery(con, "SELECT cena_sprzedazy FROM produkty WHERE id_produktu = ?", selected_product_info$id_produktu)$cena_sprzedazy
    
    kwota_zakupu <- cena_sprzedazy * ilosc_do_przeniesienia
    
    insert_query <- "
      INSERT INTO zakupy (data_zakupu, kwota_zakupu, id_produktu, miejsce_zakupu, ilosc_zakupu)
      VALUES (:data_zakupu, :kwota_zakupu, :id_produktu, :miejsce_zakupu, :ilosc_zakupu)
    "
    
    tryCatch({
      dbExecute(con, insert_query, params = list(
        data_zakupu = data_zakupu,
        kwota_zakupu = kwota_zakupu,
        id_produktu = selected_product_info$id_produktu,
        miejsce_zakupu = miejsce_sprzedazy,
        ilosc_zakupu = ilosc_do_przeniesienia
      ))
    }, error = function(e) {
      showNotification(paste("Błąd podczas dodawania zakupu:", e$message), type = "error")
      return(NULL)
    })
    
    nowa_ilosc_magazyn <- ilosc_dostepna - ilosc_do_przeniesienia
    update_query <- "
      UPDATE produkty
      SET ilosc_magazyn = :ilosc_magazyn,
          ilosc_sprzedanych = ilosc_sprzedanych + :ilosc_sprzedanych
      WHERE id_produktu = :id_produktu
    "
    
    tryCatch({
      dbExecute(con, update_query, params = list(
        ilosc_magazyn = nowa_ilosc_magazyn,
        ilosc_sprzedanych = ilosc_do_przeniesienia,
        id_produktu = selected_product_info$id_produktu
      ))
    }, error = function(e) {
      showNotification(paste("Błąd podczas aktualizacji produktu:", e$message), type = "error")
      return(NULL)
    })
    
    if (nowa_ilosc_magazyn == 0) {
      showNotification("Produkt został sprzedany w pełnej ilości i pozostaje w magazynie z ilością 0.", type = "message")
    } else {
      showNotification("Produkt został przeniesiony do sprzedanych.", type = "message")
    }
    
    removeModal()
    
    selected_product_to_transfer(NULL)
    
    output$tabelaProduktyMagazyn <- renderDT({
      produkty_magazyn <- dbGetQuery(con, "
        SELECT 
          p.id_produktu, 
          p.nazwa_produktu, 
          p.cena_sprzedazy, 
          p.ilosc_magazyn, 
          p.kolor, 
          k.nazwa_kategorii, 
          pr.nazwa_producenta
        FROM produkty p
        JOIN kategorie k ON p.id_kategorii = k.id_kategorii
        JOIN producenci pr ON p.id_producenta = pr.id_producenta
        WHERE p.ilosc_magazyn > 0
      ")
      
      datatable(
        produkty_magazyn,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })
    
    output$tabelaProduktySprzedane <- renderDT({
      sprzedane_produkty <- dbGetQuery(con, "
        SELECT 
          z.id_zakupu,
          p.id_produktu,
          p.nazwa_produktu,
          z.ilosc_zakupu,
          z.data_zakupu,
          z.kwota_zakupu,
          z.miejsce_zakupu
        FROM zakupy z
        JOIN produkty p ON z.id_produktu = p.id_produktu
      ")

      sprzedane_produkty$data_zakupu <- as.Date(sprzedane_produkty$data_zakupu)
      
      datatable(
        sprzedane_produkty,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })
  })
  
  selected_sprzedany_product <- reactiveVal(NULL)
  
  observeEvent(input$zwrot_produktu, {
    selected_row <- input$tabelaProduktySprzedane_rows_selected
    
    if (length(selected_row) == 0) {
      showNotification("Proszę wybrać sprzedany produkt do zwrotu.", type = "warning")
      return(NULL)
    }
    
    sprzedane_produkty <- dbGetQuery(con, "
      SELECT 
        z.id_zakupu,
        p.id_produktu,
        p.nazwa_produktu,
        z.ilosc_zakupu,
        z.data_zakupu,
        z.kwota_zakupu,
        z.miejsce_zakupu
      FROM zakupy z
      JOIN produkty p ON z.id_produktu = p.id_produktu
    ")
    
    selected_data <- sprzedane_produkty[selected_row, ]

    selected_sprzedany_product(list(
      id_zakupu = selected_data$id_zakupu,
      id_produktu = selected_data$id_produktu,
      nazwa_produktu = selected_data$nazwa_produktu,
      ilosc_sprzedana = selected_data$ilosc_zakupu
    ))
    
    showModal(modalDialog(
      title = paste("Zwrot produktu:", selected_data$nazwa_produktu),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Anuluj"),
        actionButton("confirm_zwrot", "Zarejestruj Zwrot", class = "btn btn-danger")
      ),
      fluidRow(
        column(12,
               tags$strong("Nazwa produktu: "), selected_data$nazwa_produktu, br(),
               tags$strong("Ilość sprzedana: "), selected_data$ilosc_zakupu, br(),
               tags$strong("Data sprzedaży: "), selected_data$data_zakupu, br(),
               tags$strong("Kwota sprzedaży: "), selected_data$kwota_zakupu, " zł", br(),
               tags$strong("Miejsce sprzedaży: "), selected_data$miejsce_zakupu, br(),
               numericInput("ilosc_zwrotu", "Ilość do zwrotu:", 
                            min = 1, max = selected_data$ilosc_zakupu, value = 1),
               selectInput("wadliwy", "Czy produkt był wadliwy?", choices = c("Nie", "Tak")),
               textAreaInput("opis_zwrotu", "Opis zwrotu:", rows = 3)
        )
      )
    ))
  })
  
  observeEvent(input$confirm_zwrot, {
        req(input$ilosc_zwrotu)
  req(input$wadliwy)
  req(input$opis_zwrotu)
  
  selected_data <- selected_sprzedany_product()
  if (is.null(selected_data)) {
    showNotification("Nie można znaleźć wybranego produktu.", type = "error")
    removeModal()
    return(NULL)
  }
  
  id_zakupu        <- selected_data$id_zakupu
  id_produktu      <- selected_data$id_produktu
  ilosc_sprzedana  <- selected_data$ilosc_sprzedana
  ilosc_zwrotu     <- input$ilosc_zwrotu
  
  if (ilosc_zwrotu > ilosc_sprzedana) {
    showNotification("Ilość zwrotu nie może przekraczać ilości sprzedanej.", type = "error")
    return(NULL)
  }
  
  wadliwy     <- ifelse(input$wadliwy == "Tak", 1, 0)
  opis_zwrotu <- input$opis_zwrotu
  
  insert_query <- "
    INSERT INTO zwroty (id_produktu, wadliwy, opis)
    VALUES (:id_produktu, :wadliwy, :opis)
  "
  dbExecute(con, insert_query, params = list(
    id_produktu = id_produktu,
    wadliwy     = wadliwy,
    opis        = opis_zwrotu
  ))
  
  update_zakupy_query <- "
    UPDATE zakupy
    SET ilosc_zakupu = ilosc_zakupu - :ilosc_zwrotu
    WHERE id_zakupu = :id_zakupu
  "
  dbExecute(con, update_zakupy_query, params = list(
    ilosc_zwrotu = ilosc_zwrotu,
    id_zakupu    = id_zakupu
  ))
  

  ilosc_pozostala_query <- "
    SELECT ilosc_zakupu 
    FROM zakupy 
    WHERE id_zakupu = :id_zakupu
  "
  ilosc_pozostala <- dbGetQuery(con, ilosc_pozostala_query, params = list(
    id_zakupu = id_zakupu
  ))
  
  if (nrow(ilosc_pozostala) == 1 && ilosc_pozostala$ilosc_zakupu == 0) {
    delete_zakupy_query <- "
      DELETE FROM zakupy
      WHERE id_zakupu = :id_zakupu
    "
    dbExecute(con, delete_zakupy_query, params = list(
      id_zakupu = id_zakupu
    ))
    showNotification("Wiersz z tabeli zakupy został usunięty (ilosc_zakupu = 0).", type = "message")
  }

  if (wadliwy == 0) {
    update_query <- "
      UPDATE produkty
      SET ilosc_magazyn = ilosc_magazyn + :ilosc_zwrotu,
          ilosc_sprzedanych = ilosc_sprzedanych - :ilosc_zwrotu
      WHERE id_produktu = :id_produktu
    "
    dbExecute(con, update_query, params = list(
      ilosc_zwrotu = ilosc_zwrotu,
      id_produktu  = id_produktu
    ))
    showNotification("Zwrot został zarejestrowany i dodany z powrotem do magazynu.", type = "message")
  } else {
    update_query <- "
      UPDATE produkty
      SET ilosc_sprzedanych = ilosc_sprzedanych - :ilosc_zwrotu
      WHERE id_produktu = :id_produktu
    "
    dbExecute(con, update_query, params = list(
      ilosc_zwrotu = ilosc_zwrotu,
      id_produktu  = id_produktu
    ))
    showNotification("Zwrot został zarejestrowany jako wadliwy.", type = "message")
  }
  
  removeModal()
    
    output$tabelaProduktyMagazyn <- renderDT({
      produkty_magazyn <- dbGetQuery(con, "
        SELECT 
          p.id_produktu, 
          p.nazwa_produktu, 
          p.cena_sprzedazy, 
          p.ilosc_magazyn, 
          p.kolor, 
          k.nazwa_kategorii, 
          pr.nazwa_producenta
        FROM produkty p
        JOIN kategorie k ON p.id_kategorii = k.id_kategorii
        JOIN producenci pr ON p.id_producenta = pr.id_producenta
        WHERE p.ilosc_magazyn > 0
      ")
      
      datatable(
        produkty_magazyn,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })
    
    output$tabelaProduktySprzedane <- renderDT({
      sprzedane_produkty <- dbGetQuery(con, "
        SELECT 
          z.id_zakupu,
          p.id_produktu,
          p.nazwa_produktu,
          z.ilosc_zakupu,
          z.data_zakupu,
          z.kwota_zakupu,
          z.miejsce_zakupu
        FROM zakupy z
        JOIN produkty p ON z.id_produktu = p.id_produktu
      ")
      
      datatable(
        sprzedane_produkty,
        selection = 'single',
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })
    
    output$tabelaZwroty <- renderDT({
      zwroty <- dbGetQuery(con, "
        SELECT 
          z.id_zwrotu,
          p.id_produktu,
          p.nazwa_produktu,
          CASE WHEN z.wadliwy = 1 THEN 'Tak' ELSE 'Nie' END AS wadliwy,
          z.opis
        FROM zwroty z
        JOIN produkty p ON z.id_produktu = p.id_produktu
      ")
      
      datatable(
        zwroty,
        selection = 'single',  
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollY = "300px",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })

    selected_sprzedany_product(NULL)
  })
  
  observeEvent(input$tabelaProduktySprzedane_rows_selected, {
    selected_row <- input$tabelaProduktySprzedane_rows_selected
    
    if (length(selected_row)) {
      sprzedane_produkty <- dbGetQuery(con, "
        SELECT 
          z.id_zakupu,
          p.id_produktu,
          p.nazwa_produktu,
          z.ilosc_zakupu,
          z.data_zakupu,
          z.kwota_zakupu,
          z.miejsce_zakupu
        FROM zakupy z
        JOIN produkty p ON z.id_produktu = p.id_produktu
      ")
      
      selected_data <- sprzedane_produkty[selected_row, ]
      
      selected_sprzedany_product(list(
        id_zakupu = selected_data$id_zakupu,
        id_produktu = selected_data$id_produktu,
        nazwa_produktu = selected_data$nazwa_produktu,
        ilosc_sprzedana = selected_data$ilosc_zakupu
      ))
      
      showModal(modalDialog(
        title = paste("Zwrot produktu:", selected_data$nazwa_produktu),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Anuluj"),
          actionButton("confirm_zwrot", "Zarejestruj Zwrot", class = "btn btn-danger")
        ),
        fluidRow(
          column(12,
                 tags$strong("Nazwa produktu: "), selected_data$nazwa_produktu, br(),
                 tags$strong("Ilość sprzedana: "), selected_data$ilosc_zakupu, br(),
                 tags$strong("Data sprzedaży: "), selected_data$data_zakupu, br(),
                 tags$strong("Kwota sprzedaży: "), selected_data$kwota_zakupu, " zł", br(),
                 tags$strong("Miejsce sprzedaży: "), selected_data$miejsce_zakupu, br(),
                 numericInput("ilosc_zwrotu", "Ilość do zwrotu:", 
                              min = 1, max = selected_data$ilosc_zakupu, value = 1),
                 selectInput("wadliwy", "Czy produkt był wadliwy?", choices = c("Nie", "Tak")),
                 textAreaInput("opis_zwrotu", "Opis zwrotu:", rows = 3)
          )
        )
      ))
    }
  })
}

  
  