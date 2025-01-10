dodajExcelModuleServer <- function(id, input, output, session, con) {
  output$download_template <- downloadHandler(
    filename = function() {
      paste("szablon_produktow", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      kategorie_df  <- dbGetQuery(con, "SELECT nazwa_kategorii FROM kategorie")
      producenci_df <- dbGetQuery(con, "SELECT nazwa_producenta FROM producenci")
      category_list <- unique(kategorie_df$nazwa_kategorii)
      producer_list <- unique(producenci_df$nazwa_producenta)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Produkty")
      
      headers <- c(
        "nazwa_produktu",
        "nazwa_kategorii",
        "cena_zakupu",
        "cena_sprzedazy",
        "kolor",
        "nazwa_producenta",
        "kod_towaru",
        "opis",
        "ilosc_magazyn",
        "data_zamowienia"   
      )
      writeData(wb, "Produkty", t(headers), startRow = 1, startCol = 1, colNames = FALSE)
      
      writeData(wb, "Produkty", category_list, startCol = 11, startRow = 3, colNames = FALSE)
      writeData(wb, "Produkty", producer_list, startCol = 12, startRow = 3, colNames = FALSE)
      
      n_cat  <- length(category_list)
      n_prod <- length(producer_list)
      cat_range  <- paste0("'Produkty'!$K$3:$K$", 2 + n_cat)
      prod_range <- paste0("'Produkty'!$L$3:$L$", 2 + n_prod)
      dataValidation(wb, "Produkty", cols = 2, rows = 2:500, type = "list", value = cat_range)
      dataValidation(wb, "Produkty", cols = 6, rows = 2:500, type = "list", value = prod_range)
      
      dataValidation(
        wb,
        sheet = "Produkty",
        cols = 10,              
        rows = 2:500,
        type = "date",
        operator = "between",
        value = as.Date(c("2000-01-01", "2100-12-31")) 
      )

      dataValidation(
        wb,
        sheet = "Produkty",
        cols = 9,              
        rows = 2:500,
        type = "whole",
        operator = "greaterThanOrEqual",
        value = 0  
      )

      dataValidation(
        wb,
        sheet = "Produkty",
        cols = 3,              
        rows = 2:500,
        type = "whole",
        operator = "greaterThanOrEqual",
        value = 0     
      )

      dataValidation(
        wb,
        sheet = "Produkty",
        cols = 4,              
        rows = 2:500,
        type = "whole",
        operator = "greaterThanOrEqual",
        value = 0    
      )

      date_style <- createStyle(numFmt = "dd.mm.yyyy")
      addStyle(wb, sheet = "Produkty", style = date_style,
               rows = 2:500, cols = 10, gridExpand = TRUE)
      
      setColWidths(wb, "Produkty", cols = 1:15, widths = "auto")
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    
    df_excel <- tryCatch(
      read_excel(input$upload_csv$datapath),
      error = function(e) {
        showNotification(paste("Błąd przy wczytywaniu pliku XLSX:", e$message), type = "error")
        return(NULL)
      }
    )
    req(df_excel)
    
    required_cols <- c(
      "nazwa_produktu",
      "nazwa_kategorii",
      "cena_zakupu",
      "cena_sprzedazy",
      "kolor",
      "nazwa_producenta",
      "kod_towaru",
      "opis",
      "ilosc_magazyn",
      "data_zamowienia"  
    )
    if (!all(required_cols %in% names(df_excel))) {
      showNotification(
        paste(
          "Brak wymaganych kolumn w XLSX. Oczekiwano:",
          paste(required_cols, collapse = ", ")
        ),
        type = "error"
      )
      return(NULL)
    }
    
    kategorie_db  <- dbGetQuery(con, "SELECT id_kategorii, nazwa_kategorii FROM kategorie")
    producenci_db <- dbGetQuery(con, "SELECT id_producenta, nazwa_producenta FROM producenci")
    
    df_joined <- df_excel %>%
      left_join(kategorie_db,   by = c("nazwa_kategorii"   = "nazwa_kategorii")) %>%
      left_join(producenci_db,  by = c("nazwa_producenta"  = "nazwa_producenta"))

    df_joined$data_zamowienia <- as.Date(df_joined$data_zamowienia, origin = "1899-12-30")
    df_joined$data_zamowienia <- as.character(df_joined$data_zamowienia)
    
    df_final <- df_joined %>%
      select(
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
      ) %>%
      filter(!is.na(nazwa_produktu) & nazwa_produktu != "")
    
    insert_query <- "
      INSERT INTO produkty
      (
        nazwa_produktu, id_kategorii, cena_zakupu, cena_sprzedazy,
        kolor, id_producenta, kod_towaru, opis, ilosc_magazyn, data_zamowienia
      )
      VALUES 
      (
        :nazwa_produktu, :id_kategorii, :cena_zakupu, :cena_sprzedazy,
        :kolor, :id_producenta, :kod_towaru, :opis, :ilosc_magazyn, :data_zamowienia
      )
    "
    dbExecute(con, insert_query, params = df_final)
    
    showNotification("Produkty zostały wgrane i zmapowane pomyślnie!", type = "message")
    
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
  })
}
