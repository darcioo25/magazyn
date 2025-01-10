library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)

porownajModuleServer <- function(id, input, output, session, con) {
    kategorie_df <- reactive({
      dbGetQuery(
        con,
        "
        SELECT 
          k.id_kategorii, 
          k.nazwa_kategorii
        FROM kategorie k
        JOIN produkty p ON p.id_kategorii = k.id_kategorii
        LEFT JOIN zakupy z ON p.id_produktu = z.id_produktu
        GROUP BY k.id_kategorii
        HAVING (COALESCE(SUM(z.ilosc_zakupu), 0) > 0 
                OR COALESCE(SUM(p.ilosc_magazyn), 0) > 0)
        "
      )
    })
    
    observeEvent(kategorie_df(), {
      df <- kategorie_df()
      updateSelectInput(
        session = session,
        inputId = "porownaj_kategoria",
        choices = setNames(df$id_kategorii, df$nazwa_kategorii)
      )
    })
    
    produkty_wybranej_kategorii <- reactive({
      req(input$porownaj_kategoria)
      
      query <- paste0("
        SELECT
          p.id_produktu,
          p.nazwa_produktu,
          p.id_kategorii,
          p.cena_zakupu,
          p.cena_sprzedazy,
          p.kolor,
          p.id_producenta,
          p.kod_towaru,
          p.opis,
          p.ilosc_magazyn,
          
          -- Liczba sprzedanych sztuk = suma ilosc_zakupu z zakupy
          COALESCE(SUM(z.ilosc_zakupu), 0) AS ilosc_sprzedanych,
          
          -- (opcjonalnie) łączna kwota zakupu 
          COALESCE(SUM(z.kwota_zakupu), 0) AS laczna_kwota_zakupu
          
        FROM produkty p
          LEFT JOIN zakupy z ON p.id_produktu = z.id_produktu
        WHERE p.id_kategorii = ", input$porownaj_kategoria, "
        GROUP BY p.id_produktu
        HAVING ( COALESCE(SUM(z.ilosc_zakupu), 0) > 0 
                 OR COALESCE(p.ilosc_magazyn, 0) > 0 )
      ")
      
      dbGetQuery(con, query)
    })
    
    output$porownaj_produkty_ui <- renderUI({
      df <- produkty_wybranej_kategorii()
      
      if (nrow(df) == 0) {
        return(p("Brak produktów w wybranej kategorii."))
      }
      
      selectInput(
        inputId = "porownaj_produkty",
        label = "Wybierz maks. 2 produkty do porównania:",
        choices = setNames(df$id_produktu, df$nazwa_produktu),
        multiple = TRUE,
        selectize = TRUE 
      )
    })
    
    output$porownaj_wykres <- renderPlot({
      df <- produkty_wybranej_kategorii()
      req(nrow(df) > 0)
      
      ggplot(df, aes(x = reorder(nazwa_produktu, -ilosc_sprzedanych), 
                     y = ilosc_sprzedanych)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
          x = "Produkt",
          y = "Liczba sprzedanych sztuk"
        ) +
        theme_minimal(base_size = 14)
    })
    
    output$porownaj_bestworst <- renderUI({
      df <- produkty_wybranej_kategorii()
      if (nrow(df) == 0) {
        return(HTML("<p>Brak produktów w wybranej kategorii.</p>"))
      }

      df_sorted <- df[order(-df$ilosc_sprzedanych), ]
      best  <- df_sorted[1, ]
      worst <- df_sorted[nrow(df_sorted), ]
      
      HTML(
        paste0(
          "<b>Najlepiej sprzedający się produkt:</b> ", best$nazwa_produktu,
          " (", best$ilosc_sprzedanych, " sprzedanych)<br/>",
          "<b>Najgorzej sprzedający się produkt:</b> ", worst$nazwa_produktu,
          " (", worst$ilosc_sprzedanych, " sprzedanych)<br/>"
        )
      )
    })

    output$box_prod1 <- renderUI({
    prod_ids <- input$porownaj_produkty
    if (is.null(prod_ids) || length(prod_ids) < 1) {
        return(NULL)
    }
    
    df <- produkty_wybranej_kategorii()
    df1 <- df[df$id_produktu == prod_ids[1], ]
    if (nrow(df1) == 0) {
        return(NULL)
    }
    
    box(
        title = "Produkt 1",
        width = 12,
        tableOutput("porownaj_tabela_prod1")
    )
    })

    output$box_prod2 <- renderUI({
    prod_ids <- input$porownaj_produkty
    if (is.null(prod_ids) || length(prod_ids) < 2) {
        return(NULL)
    }
    
    df <- produkty_wybranej_kategorii()
    df2 <- df[df$id_produktu == prod_ids[2], ]
    if (nrow(df2) == 0) {
        return(NULL)
    }
    
    box(
        title = "Produkt 2",
        width = 12,
        tableOutput("porownaj_tabela_prod2")
    )
    })

    
    output$porownaj_tabela_prod1 <- renderTable({
      prod_ids <- input$porownaj_produkty
      if (is.null(prod_ids) || length(prod_ids) < 1) return()
      
      df <- produkty_wybranej_kategorii()
      df1 <- df[df$id_produktu == prod_ids[1], ]
      if (nrow(df1) == 0) return()
      
      data.frame(
        Parametr = c(
          "Nazwa", 
          "Cena zakupu", 
          "Cena sprzedaży", 
          "Kolor",
          "Ilość na magazynie", 
          "Ilość sprzedana (z zakupy)", 
          "Łączna kwota zakupów (z zakupy)"
        ),
        Wartość = c(
          df1$nazwa_produktu,
          df1$cena_zakupu,
          df1$cena_sprzedazy,
          df1$kolor,
          df1$ilosc_magazyn,
          df1$ilosc_sprzedanych,
          df1$laczna_kwota_zakupu
        )
      )
    }, striped = TRUE, spacing = "xs")
    
    output$porownaj_tabela_prod2 <- renderTable({
      prod_ids <- input$porownaj_produkty
      if (is.null(prod_ids) || length(prod_ids) < 2) return()
      
      df <- produkty_wybranej_kategorii()
      df2 <- df[df$id_produktu == prod_ids[2], ]
      if (nrow(df2) == 0) return()
      
      data.frame(
        Parametr = c(
          "Nazwa", 
          "Cena zakupu", 
          "Cena sprzedaży", 
          "Kolor",
          "Ilość na magazynie", 
          "Ilość sprzedana (z zakupy)", 
          "Łączna kwota zakupów (z zakupy)"
        ),
        Wartość = c(
          df2$nazwa_produktu,
          df2$cena_zakupu,
          df2$cena_sprzedazy,
          df2$kolor,
          df2$ilosc_magazyn,
          df2$ilosc_sprzedanych,
          df2$laczna_kwota_zakupu
        )
      )
    }, striped = TRUE, spacing = "xs")
}
