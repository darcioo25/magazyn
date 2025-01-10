rekomendacjeModuleServer <- function(id, input, output, session, con) {
  output$rekomendacje <- renderUI({

    recommendations <- list()
    current_date <- Sys.Date()

    possible_places <- data.frame(
      miejsce_zakupu = c("Poznań", "Warszawa", "Gdańsk", "Kraków", "Internet"),
      stringsAsFactors = FALSE
    )
    
    city_sales <- dbGetQuery(con, "
      SELECT 
        miejsce_zakupu,
        COALESCE(SUM(kwota_zakupu), 0) AS total_sales
      FROM zakupy
      GROUP BY miejsce_zakupu
    ")
    
    city_sales <- merge(possible_places, city_sales, by = "miejsce_zakupu", all.x = TRUE)
    city_sales$total_sales[is.na(city_sales$total_sales)] <- 0

    zero_sales_cities <- city_sales %>%
      dplyr::filter(total_sales == 0)

    worst_sales_store <- dbGetQuery(con, "
      SELECT 
        miejsce_zakupu,
        SUM(kwota_zakupu) AS total_sales
      FROM zakupy
      GROUP BY miejsce_zakupu
      ORDER BY total_sales ASC
      LIMIT 1
    ")

    if(nrow(zero_sales_cities) > 0){
      store_recom <- list(
        type = "zero_sales_cities", 
        id = "zero_sales",
        title = "Punkty z zerową sprzedażą",
        description = paste0(
          "Wystąpiły następujące miejsca z zerową sprzedażą: ",
          paste(zero_sales_cities$miejsce_zakupu, collapse = ", ")
        ),
        more_info = paste(
          "Zalecamy wprowadzenie dodatkowych rabatów, inwestycje w marketing,",
          "lub rozważenie zamknięcia tych punktów, aby uniknąć generowania kosztów."
        )
      )
      recommendations <- append(list(store_recom), recommendations)

    } else if(nrow(worst_sales_store) > 0){
      store_recom <- list(
        type = "lowest_store_sales", 
        id = worst_sales_store$miejsce_zakupu[1],  
        title = paste("Sklep z najgorszą sprzedażą:", worst_sales_store$miejsce_zakupu[1]),
        description = paste0(
          "Sklep ten osiągnął łączną sprzedaż na poziomie: ", 
          round(worst_sales_store$total_sales[1], 2), " zł."
        ),
        more_info = paste(
          "Zalecamy wprowadzenie dodatkowych zniżek, zainwestowanie w marketing",
          "oraz rozważenie kampanii promocyjnych, aby zwiększyć sprzedaż w tym sklepie."
        )
      )
      recommendations <- append(list(store_recom), recommendations)
    }

    best_selling <- dbGetQuery(con, "
      SELECT 
        p.id_produktu,
        p.nazwa_produktu,
        COALESCE(SUM(z.ilosc_zakupu), 0) AS total_sold,
        p.ilosc_magazyn
      FROM produkty p
      LEFT JOIN zakupy z ON p.id_produktu = z.id_produktu
      GROUP BY p.id_produktu
      ORDER BY total_sold DESC
      LIMIT 1
    ")
    
    if(nrow(best_selling) == 1) {
      if(best_selling$total_sold[1] > 0) {
        best_selling_recom <- list(
          type = "best_selling",
          id = best_selling$id_produktu[1],
          title = paste("Zamów więcej produktu:", best_selling$nazwa_produktu[1]),
          description = paste0(
            "Najlepiej sprzedający się produkt to '", 
            best_selling$nazwa_produktu[1], 
            "' (", best_selling$total_sold[1], " sprzedanych sztuk)."
          ),
          more_info = paste(
            "Zalecamy rozważyć zamówienie dodatkowych ilości tego produktu,",
            "ponieważ jest on najchętniej kupowany przez klientów."
          )
        )
        recommendations <- append(recommendations, list(best_selling_recom))
      }
    }

    next_special_day <- dbGetQuery(con, "
      SELECT *
      FROM dni_specjalne
      WHERE data_zakonczenia >= ?
      ORDER BY data_rozpoczecia ASC
      LIMIT 1
    ", params = list(current_date))
    
    if(nrow(next_special_day) > 0){
      special_day <- next_special_day[1, ]

      if(special_day$nazwa == "Komunie") {
        special_promotion <- list(
          type = "special_day_promotion",
          id = special_day$id,
          title = paste("Promocja na okazję Komunii Świętej"),
          description = paste(
            "Zbliża się okres Komunii Świętych!", 
            "Zachęcamy do przygotowania specjalnych ofert na prezenty, takie jak tablety, telefony i smartwatche, które będą idealne na tę okazję."
          ),
          more_info = paste(
            "Polecamy skoncentrować się na promocjach w okresie od", 
            format(as.Date(special_day$data_rozpoczecia), "%d-%m-%Y"), "do", 
            format(as.Date(special_day$data_zakonczenia), "%d-%m-%Y"), 
            ". Warto rozważyć zniżki, darmową dostawę lub eleganckie opakowania prezentowe."
          )
        )
      } else {
        special_promotion <- list(
          type = "special_day_promotion",
          id = special_day$id,
          title = paste("Promocja na", special_day$nazwa),
          description = paste(
            "Zbliża się", special_day$nazwa, 
            "! Zorganizuj promocje na telefony, tablety i laptopy, aby przyciągnąć więcej klientów."
          ),
          more_info = paste(
            "Zalecamy przygotowanie atrakcyjnych ofert na telefony, tablety i laptopy w okresie od", 
            format(as.Date(special_day$data_rozpoczecia), "%d-%m-%Y"), "do", 
            format(as.Date(special_day$data_zakonczenia), "%d-%m-%Y"), 
            ". Możesz zastosować rabaty, oferty pakietowe lub dodatkowe bonusy dla klientów."
          )
        )
      }

      recommendations <- append(list(special_promotion), recommendations)
    }
    
    five_months_ago <- lubridate::floor_date(current_date, "month") %m-% months(5)
    five_months_ago_str <- format(five_months_ago, "%Y-%m-%d")  
    producer_sales <- dbGetQuery(con, "
      SELECT 
          pr.id_producenta,
          pr.nazwa_producenta,
          COALESCE(SUM(CASE WHEN z.data_zakupu >= ? THEN z.ilosc_zakupu ELSE 0 END), 0) AS sprzedane_ostatnie5m,
          COALESCE(SUM(z.ilosc_zakupu), 0) AS sprzedane_calkowite
      FROM 
          producenci pr
      INNER JOIN 
          produkty p ON p.id_producenta = pr.id_producenta
      LEFT JOIN 
          zakupy z ON z.id_produktu = p.id_produktu
      GROUP BY 
          pr.id_producenta, 
          pr.nazwa_producenta
    ", params = list(five_months_ago_str))
    
    producer_sales <- producer_sales %>%
      dplyr::mutate(procent_5m = ifelse(sprzedane_calkowite > 0,
                                 (sprzedane_ostatnie5m / sprzedane_calkowite) * 100,
                                 0))
    producers_need_day <- producer_sales %>% dplyr::filter(procent_5m < 40)
    
    if(nrow(producers_need_day) > 0){
      for(i in seq_len(nrow(producers_need_day))){
        producer <- producers_need_day[i, ]
        recommendations <- append(recommendations, list(
          list(
            type = "producer_day",
            id = producer$id_producenta,
            title = paste(producer$nazwa_producenta, "Day"),
            description = paste("Sprzedaż produktów producenta", 
                                producer$nazwa_producenta,
                                "wynosi tylko",
                                round(producer$procent_5m, 2),
                                "% w ostatnich 5 miesiącach."),
            more_info = paste(
              "Zalecamy zorganizowanie", 
              producer$nazwa_producenta, 
              "Day. Producent powinien zastosować strategię zwiększenia atrakcyjności produktów, na przykład poprzez dołożenie słuchawek do telefonów, nałożenie kilku promocji, czy organizację wydarzeń specjalnych."
            )
          )
        ))
      }
    }
    
    product_sales <- dbGetQuery(con, "
      SELECT 
          p.id_produktu,
          p.nazwa_produktu,
          p.ilosc_magazyn,
          p.data_zamowienia,
          COALESCE(SUM(z.ilosc_zakupu), 0) AS suma_sprzedana
      FROM 
          produkty p
      LEFT JOIN 
          zakupy z ON z.id_produktu = p.id_produktu
      GROUP BY 
          p.id_produktu, 
          p.nazwa_produktu, 
          p.ilosc_magazyn,
          p.data_zamowienia
    ")
    
    product_sales$data_zamowienia <- as.Date(product_sales$data_zamowienia, format = "%Y-%m-%d")
    
    if(any(is.na(product_sales$data_zamowienia))) {
      warning("Niektóre produkty mają nieprawidłowe daty przyjęcia do magazynu.")
    }
    
    product_sales <- product_sales %>%
      dplyr::mutate(
        dni_w_magazynie = as.numeric(current_date - data_zamowienia),
        ilosc_startowa  = ilosc_magazyn + suma_sprzedana,
        procent_sprzedazy = ifelse(
          ilosc_startowa > 0,
          (suma_sprzedana / ilosc_startowa) * 100,
          0
        ),
        discount = dplyr::case_when(
          dni_w_magazynie > 90 & procent_sprzedazy <= 10 ~ 20,
          dni_w_magazynie > 60 & procent_sprzedazy <= 10 ~ 10,
          dni_w_magazynie > 30 & procent_sprzedazy <= 10 ~ 5,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::filter(!is.na(discount))
    
    if(nrow(product_sales) > 0){
      for(i in seq_len(nrow(product_sales))){
        prod <- product_sales[i, ]
        
        info_text <- switch(
          as.character(prod$discount),
          `5`  = "Zalecamy zastosowanie zniżki 5%. Produkt nie przekroczył 10% sprzedaży przez ponad 1 miesiąc.",
          `10` = "Zalecamy zastosowanie zniżki 10%. Produkt nie przekroczył 10% sprzedaży przez ponad 2 miesiące.",
          `20` = "Zalecamy zastosowanie zniżki 20%. Produkt nie przekroczył 10% sprzedaży przez ponad 3 miesiące."
        )
        
        recommendations <- append(recommendations, list(
          list(
            type = "product_discount",
            id = prod$id_produktu,
            title = paste("Zniżka dla:", prod$nazwa_produktu),
            description = paste0(
              "Minęło ", prod$dni_w_magazynie, " dni od przyjęcia do magazynu. ",
              "Sprzedano tylko ", round(prod$procent_sprzedazy, 2), "% dostępnego towaru."
            ),
            more_info = info_text
          )
        ))
      }
    }
    
    if(length(recommendations) == 0){
      return(
        fluidRow(
          box(
            title = "Brak aktualnych rekomendacji.",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            "Aktualnie nie ma rekomendacji do wyświetlenia."
          )
        )
      )
    }
    
    rekomendacje_ui <- lapply(recommendations, function(recom){
      box(
        title = recom$title,
        status = if(recom$type == "producer_day") "warning" else "success",
        solidHeader = TRUE,
        width = 12,
        p(recom$description),
        actionButton(
          inputId = paste0("info_", recom$type, "_", recom$id),
          label = "Więcej informacji",
          class = "btn btn-primary"
        )
      )
    })
    
    do.call(tagList, rekomendacje_ui)
  })
  
  create_modal_observer <- function(recom){
    observeEvent(input[[paste0("info_", recom$type, "_", recom$id)]], {
      showModal(modalDialog(
        title = recom$title,
        size = "m",
        easyClose = TRUE,
        footer = modalButton("Zamknij"),
        p(recom$more_info)
      ))
    }, ignoreInit = TRUE)
  }

  observe({
    recommendations <- list()
    current_date <- Sys.Date()
    possible_places <- data.frame(
      miejsce_zakupu = c("Poznań", "Warszawa", "Gdańsk", "Kraków", "Internet"),
      stringsAsFactors = FALSE
    )
    
    city_sales <- dbGetQuery(con, "
      SELECT 
        miejsce_zakupu,
        COALESCE(SUM(kwota_zakupu), 0) AS total_sales
      FROM zakupy
      GROUP BY miejsce_zakupu
    ")
    
    city_sales <- merge(possible_places, city_sales, by = "miejsce_zakupu", all.x = TRUE)
    city_sales$total_sales[is.na(city_sales$total_sales)] <- 0
    
    zero_sales_cities <- city_sales %>%
      dplyr::filter(total_sales == 0)

    worst_sales_store <- dbGetQuery(con, "
      SELECT 
        miejsce_zakupu,
        SUM(kwota_zakupu) AS total_sales
      FROM zakupy
      GROUP BY miejsce_zakupu
      ORDER BY total_sales ASC
      LIMIT 1
    ")

    if (nrow(zero_sales_cities) > 0) {
      store_recom <- list(
        type = "zero_sales_cities",
        id = "zero_sales",
        title = "Punkty z zerową sprzedażą",
        description = paste0(
          "Wystąpiły następujące miejsca z zerową sprzedażą: ",
          paste(zero_sales_cities$miejsce_zakupu, collapse = ", ")
        ),
        more_info = paste(
          "Zalecamy wprowadzenie dodatkowych rabatów, inwestycje w marketing,",
          "lub rozważenie zamknięcia tych punktów, aby uniknąć generowania kosztów."
        )
      )
      recommendations <- append(list(store_recom), recommendations)

    } else if (nrow(worst_sales_store) > 0) {
      store_recom <- list(
        type = "lowest_store_sales",
        id = worst_sales_store$miejsce_zakupu[1],
        title = paste("Sklep z najgorszą sprzedażą:", worst_sales_store$miejsce_zakupu[1]),
        description = paste0(
          "Sklep ten osiągnął łączną sprzedaż na poziomie: ", 
          round(worst_sales_store$total_sales[1], 2), " zł."
        ),
        more_info = paste(
          "Zalecamy wprowadzenie dodatkowych zniżek, zainwestowanie w marketing",
          "oraz rozważenie kampanii promocyjnych, aby zwiększyć sprzedaż w tym sklepie."
        )
      )
      recommendations <- append(list(store_recom), recommendations)
    }

    best_selling <- dbGetQuery(con, "
      SELECT 
        p.id_produktu,
        p.nazwa_produktu,
        COALESCE(SUM(z.ilosc_zakupu), 0) AS total_sold,
        p.ilosc_magazyn
      FROM produkty p
      LEFT JOIN zakupy z ON p.id_produktu = z.id_produktu
      GROUP BY p.id_produktu
      ORDER BY total_sold DESC
      LIMIT 1
    ")
    
    if(nrow(best_selling) == 1) {
      if(best_selling$total_sold[1] > 0) {
        best_selling_recom <- list(
          type = "best_selling",
          id = best_selling$id_produktu[1],
          title = paste("Zamów więcej produktu:", best_selling$nazwa_produktu[1]),
          description = paste0(
            "Najlepiej sprzedający się produkt to '", 
            best_selling$nazwa_produktu[1], 
            "' (", best_selling$total_sold[1], " sprzedanych sztuk)."
          ),
          more_info = paste(
            "Zalecamy rozważyć zamówienie dodatkowych ilości tego produktu,",
            "ponieważ jest on najchętniej kupowany przez klientów."
          )
        )
        recommendations <- append(recommendations, list(best_selling_recom))
      }
    }

    next_special_day <- dbGetQuery(con, "
      SELECT *
      FROM dni_specjalne
      WHERE data_zakonczenia >= ?
      ORDER BY data_rozpoczecia ASC
      LIMIT 1
    ", params = list(current_date))
    
    if(nrow(next_special_day) > 0){
    special_day <- next_special_day[1, ]

    if(special_day$nazwa == "Komunia") {
        special_promotion <- list(
            type = "special_day_promotion",
            id = special_day$id,
            title = paste("Promocja na okazję Komunii Świętej"),
            description = paste(
            "Zbliża się Komunia Święta!", 
            "Zachęcamy do przygotowania specjalnych ofert na prezenty, takie jak tablety, telefony i smartwatche, które będą skierowane dla rodziców."
            ),
            more_info = paste(
            "Polecamy skoncentrować się na promocjach w okresie od", 
            format(as.Date(special_day$data_rozpoczecia), "%d-%m-%Y"), "do", 
            format(as.Date(special_day$data_zakonczenia), "%d-%m-%Y"), 
            ". Warto rozważyć zniżki, darmową dostawę lub eleganckie opakowania prezentowe."
            )
        )
    } else {
        special_promotion <- list(
            type = "special_day_promotion",
            id = special_day$id,
            title = paste("Promocja na", special_day$nazwa),
            description = paste(
            "Zbliża się", special_day$nazwa, 
            "! Zorganizuj promocje na telefony, tablety i laptopy, aby przyciągnąć więcej klientów."
            ),
            more_info = paste(
            "Zalecamy przygotowanie atrakcyjnych ofert na telefony, tablety i laptopy w okresie od", 
            format(as.Date(special_day$data_rozpoczecia), "%d-%m-%Y"), "do", 
            format(as.Date(special_day$data_zakonczenia), "%d-%m-%Y"), 
            ". Możesz zastosować rabaty, oferty pakietowe lub dodatkowe bonusy dla klientów."
            )
        )
    }

    recommendations <- append(list(special_promotion), recommendations)
    }

    
    five_months_ago <- lubridate::floor_date(current_date, "month") %m-% months(5)
    five_months_ago_str <- format(five_months_ago, "%Y-%m-%d")  
    
    producer_sales <- dbGetQuery(con, "
      SELECT 
          pr.id_producenta,
          pr.nazwa_producenta,
          COALESCE(SUM(CASE WHEN z.data_zakupu >= ? THEN z.ilosc_zakupu ELSE 0 END), 0) AS sprzedane_ostatnie5m,
          COALESCE(SUM(z.ilosc_zakupu), 0) AS sprzedane_calkowite
      FROM 
          producenci pr
      INNER JOIN 
          produkty p ON p.id_producenta = pr.id_producenta
      LEFT JOIN 
          zakupy z ON z.id_produktu = p.id_produktu
      GROUP BY 
          pr.id_producenta, 
          pr.nazwa_producenta
    ", params = list(five_months_ago_str))
    
    producer_sales <- producer_sales %>%
      dplyr::mutate(procent_5m = dplyr::if_else(
        sprzedane_calkowite > 0,
        (sprzedane_ostatnie5m / sprzedane_calkowite) * 100,
        0
      ))
    
    producers_need_day <- producer_sales %>% dplyr::filter(procent_5m < 40)
    if(nrow(producers_need_day) > 0){
      for(i in seq_len(nrow(producers_need_day))){
        producer <- producers_need_day[i, ]
        recommendations <- append(recommendations, list(
          list(
            type = "producer_day",
            id = producer$id_producenta,
            title = paste(producer$nazwa_producenta, "Day"),
            description = paste(
              "Sprzedaż produktów producenta", 
              producer$nazwa_producenta, "wynosi tylko",
              round(producer$procent_5m, 2), "% w ostatnich 5 miesiącach."
            ),
            more_info = paste(
              "Zalecamy zorganizowanie", producer$nazwa_producenta, 
              "Day. Producent powinien zastosować strategię zwiększenia atrakcyjności produktów."
            )
          )
        ))
      }
    }
    
    product_sales <- dbGetQuery(con, "
      SELECT 
          p.id_produktu,
          p.nazwa_produktu,
          p.ilosc_magazyn,
          p.data_zamowienia,
          COALESCE(SUM(z.ilosc_zakupu), 0) AS suma_sprzedana
      FROM 
          produkty p
      LEFT JOIN 
          zakupy z ON z.id_produktu = p.id_produktu
      GROUP BY 
          p.id_produktu, 
          p.nazwa_produktu, 
          p.ilosc_magazyn,
          p.data_zamowienia
    ")
    
    product_sales$data_zamowienia <- as.Date(product_sales$data_zamowienia, format = "%Y-%m-%d")
    
    if(any(is.na(product_sales$data_zamowienia))) {
      warning("Niektóre produkty mają nieprawidłowe daty przyjęcia do magazynu.")
    }
    
    product_sales <- product_sales %>%
      dplyr::mutate(
        dni_w_magazynie = as.numeric(current_date - data_zamowienia),
        ilosc_startowa = ilosc_magazyn + suma_sprzedana,
        procent_sprzedazy = ifelse(
          ilosc_startowa > 0,
          (suma_sprzedana / ilosc_startowa) * 100,
          0
        ),
        discount = dplyr::case_when(
          dni_w_magazynie > 90 & procent_sprzedazy <= 10 ~ 20,
          dni_w_magazynie > 60 & procent_sprzedazy <= 10 ~ 10,
          dni_w_magazynie > 30 & procent_sprzedazy <= 10 ~ 5,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::filter(!is.na(discount))
    
    if(nrow(product_sales) > 0){
      for(i in seq_len(nrow(product_sales))){
        prod <- product_sales[i, ]
        info_text <- switch(
          as.character(prod$discount),
          `5`  = "Zalecamy zastosowanie zniżki 5%. Produkt nie przekroczył 10% sprzedaży przez ponad 1 miesiąc.",
          `10` = "Zalecamy zastosowanie zniżki 10%. Produkt nie przekroczył 10% sprzedaży przez ponad 2 miesiące.",
          `20` = "Zalecamy zastosowanie zniżki 20%. Produkt nie przekroczył 10% sprzedaży przez ponad 3 miesiące."
        )
        recommendations <- append(recommendations, list(
          list(
            type = "product_discount",
            id = prod$id_produktu,
            title = paste("Zniżka dla:", prod$nazwa_produktu),
            description = paste0(
              "Minęło ", prod$dni_w_magazynie, " dni od przyjęcia do magazynu. ",
              "Sprzedano tylko ", round(prod$procent_sprzedazy, 2), "% dostępnego towaru."
            ),
            more_info = info_text
          )
        ))
      }
    }

    if(length(recommendations) > 0){
      lapply(recommendations, create_modal_observer)
    }
  })
}
