library(shiny)
library(DBI)
library(dplyr)
library(lubridate)
library(ggplot2)

dashboardModuleServer <- function(id, input, output, session, con) {
    sales_locations <- data.frame(
      miejsce_zakupu = c("Poznań", "Warszawa", "Gdańsk", "Kraków"),
      lat = c(52.4064, 52.2297, 54.3520, 50.0647),
      lng = c(16.9252, 21.0122, 18.6466, 19.9450)
    )
    aggregated_sales <- reactive({
      req(input$aggregation) 
      
      current_date <- Sys.Date()
      
      aggregated_data <- NULL
      
      query <- "
        SELECT 
          z.data_zakupu,
          z.ilosc_zakupu,
          p.cena_zakupu,
          p.cena_sprzedazy
        FROM 
          zakupy z
        INNER JOIN 
          produkty p ON z.id_produktu = p.id_produktu
      "
      
      sales_data <- dbGetQuery(con, query)

      sales_data$data_zakupu <- as.Date(sales_data$data_zakupu)
      
      if(nrow(sales_data) == 0){
        return(NULL)
      }
      
      aggregated_data <- sales_data %>%
        mutate(period = case_when(
          input$aggregation == "week" ~ floor_date(data_zakupu, "week"),
          input$aggregation == "month" ~ floor_date(data_zakupu, "month"),
          input$aggregation == "year" ~ floor_date(data_zakupu, "year")
        ),
        margin = (cena_sprzedazy - cena_zakupu) * ilosc_zakupu
        ) %>%
        group_by(period) %>%
        summarise(total_sales = sum(margin, na.rm = TRUE)) %>%
        ungroup()
      
      return(aggregated_data)
    })
    
    output$sales_plot <- renderPlot({
      data <- aggregated_sales()
      
      if(is.null(data) || nrow(data) == 0){
        plot.new()
        text(0.5, 0.5, "Brak danych do wyświetlenia.", cex = 1.5)
        return()
      }

    ggplot(data, aes(x = period, y = total_sales)) +
      geom_col(fill = "steelblue") + 
      labs(
        title = "Sprzedaż",
        x = "Okres",
        y = "Całkowita Sprzedaż (PLN)"
      ) +
      theme_minimal()

    })

  best_and_worst_sellers <- reactive({
    query <- "
      SELECT 
        p.id_produktu,
        p.nazwa_produktu,
        SUM(z.ilosc_zakupu) AS total_quantity
      FROM 
        zakupy z
      INNER JOIN 
        produkty p ON z.id_produktu = p.id_produktu
      GROUP BY 
        p.id_produktu,
        p.nazwa_produktu
      ORDER BY 
        total_quantity DESC
    "
    
    product_sales <- dbGetQuery(con, query)
    
    if(nrow(product_sales) == 0){
      return(list(best = NULL, worst = NULL))
    }
    
    best_seller <- product_sales[1, ]
    
    worst_seller <- product_sales[nrow(product_sales), ]
    
    return(list(best = best_seller, worst = worst_seller))
  })
  
  output$best_seller <- renderUI({
    sellers <- best_and_worst_sellers()
    
    if(is.null(sellers$best)){
      HTML("<strong>Brak danych</strong>")
    } else {
      HTML(paste0(
        "<strong>", sellers$best$nazwa_produktu, "</strong><br>",
        "Ilość sprzedanych jednostek: ", sellers$best$total_quantity
      ))
    }
  })
  
  output$worst_seller <- renderUI({
    sellers <- best_and_worst_sellers()
    
    if(is.null(sellers$worst)){
      HTML("<strong>Brak danych</strong>")
    } else {
      HTML(paste0(
        "<strong>", sellers$worst$nazwa_produktu, "</strong><br>",
        "Ilość sprzedanych jednostek: ", sellers$worst$total_quantity
      ))
    }
  })

  output$quantity <- renderUI({
      query <- "
        SELECT 
          SUM(ilosc_zakupu) AS sale_quantity
        FROM 
          zakupy
      "
      
      sales_quantity <- dbGetQuery(con, query)
    
    if(is.null(sales_quantity$sale_quantity)){
      HTML("<strong>Brak danych</strong>")
    } else {
      HTML(paste0(
        "Ilość sprzedanych produktów: ", sales_quantity$sale_quantity
      ))
    }
  })

    output$warehouse_quantity <- renderUI({
      query <- "
        SELECT 
          SUM(ilosc_magazyn) AS war_quantity
        FROM 
          produkty
      "
      
      warehouse_product_quantity <- dbGetQuery(con, query)
    
    if(is.null(warehouse_product_quantity$war_quantity)){
      HTML("<strong>Brak danych</strong>")
    } else {
      HTML(paste0(
        "Ilość produktów na magazynie: ", warehouse_product_quantity$war_quantity
      ))
    }
  })

  output$sales_map <- renderLeaflet({
  query <- "
    SELECT 
      miejsce_zakupu,
      SUM(ilosc_zakupu) AS total_quantity
    FROM 
      zakupy
    GROUP BY 
      miejsce_zakupu
  "
  
  location_sales <- dbGetQuery(con, query)
  
  if (nrow(location_sales) == 0) {
    return(NULL)
  }
  
  map_data <- merge(sales_locations, location_sales, by = "miejsce_zakupu", all.x = TRUE)
  map_data$total_quantity[is.na(map_data$total_quantity)] <- 0
  
  leaflet(map_data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~lng,
      lat = ~lat,
      label = ~paste0(
        miejsce_zakupu, '\n',
        'Ilość sprzedanych produktów: ', total_quantity
      ),
      radius = ~sqrt(total_quantity) * 2,  
      color = "blue",
      fill = TRUE,
      fillOpacity = 0.6,
      popup = ~paste0(
        '<strong>', miejsce_zakupu, '</strong><br>',
        'Ilość sprzedanych produktów: ', total_quantity
      )
    )
})

  sales_by_producer <- reactive({
    query <- "
      SELECT 
        pr.nazwa_producenta,
        SUM(z.ilosc_zakupu) AS total_quantity
      FROM 
        zakupy z
      INNER JOIN 
        produkty p ON z.id_produktu = p.id_produktu
      INNER JOIN
        producenci pr ON p.id_producenta = pr.id_producenta
      GROUP BY 
        pr.nazwa_producenta
      ORDER BY 
        total_quantity DESC
    "
    
    producer_sales <- dbGetQuery(con, query)
    
    if(nrow(producer_sales) == 0){
      return(NULL)
    }
    
    return(producer_sales)
  })

  sales_by_category <- reactive({
    query <- "
      SELECT 
        k.nazwa_kategorii,
        SUM(z.ilosc_zakupu) AS total_quantity
      FROM 
        zakupy z
      INNER JOIN 
        produkty p ON z.id_produktu = p.id_produktu
      INNER JOIN
        kategorie k ON p.id_kategorii = k.id_kategorii
      GROUP BY 
        k.nazwa_kategorii
      ORDER BY 
        total_quantity DESC
    "
    
    category_sales <- dbGetQuery(con, query)
    
    if(nrow(category_sales) == 0){
      return(NULL)
    }
    
    return(category_sales)
  })

  sales_by_location <- reactive({
    query <- "
      SELECT 
        miejsce_zakupu,
        SUM(kwota_zakupu) AS total_sales
      FROM 
        zakupy
      GROUP BY 
        miejsce_zakupu
      ORDER BY 
        total_sales DESC
    "
    
    location_sales <- dbGetQuery(con, query)
    
    if (nrow(location_sales) == 0) {
      return(NULL)
    }
    
    return(location_sales)
  })

  output$sales_producer_plot <- renderPlot({
    data <- sales_by_producer()
    
    if(is.null(data) || nrow(data) == 0){
      plot.new()
      text(0.5, 0.5, "Brak danych do wyświetlenia.", cex = 1.5)
      return()
    }
    
    ggplot(data, aes(x = reorder(nazwa_producenta, -total_quantity), y = total_quantity)) +
      geom_col(fill = "darkgreen") +
      labs(
        title = "Sprzedaż na Producentów",
        x = "Producent",
        y = "Ilość Sprzedanych Jednostek"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$sales_category_plot <- renderPlot({
    data <- sales_by_category()
    
    if(is.null(data) || nrow(data) == 0){
      plot.new()
      text(0.5, 0.5, "Brak danych do wyświetlenia.", cex = 1.5)
      return()
    }
    
    ggplot(data, aes(x = reorder(nazwa_kategorii, -total_quantity), y = total_quantity)) +
      geom_col(fill = "purple") +
      labs(
        title = "Sprzedaż na Kategorie",
        x = "Kategoria",
        y = "Ilość Sprzedanych Jednostek"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$sales_location_plot <- renderPlot({
  data <- sales_by_location()
  
  if (is.null(data) || nrow(data) == 0) {
    plot.new()
    text(0.5, 0.5, "Brak danych do wyświetlenia.", cex = 1.5)
    return()
  }
  
  ggplot(data, aes(x = reorder(miejsce_zakupu, -total_sales), y = total_sales)) +
    geom_col(fill = "orange") + 
    labs(
      title = "Sprzedaż na Miejsca",
      x = "Miejsce Zakupu",
      y = "Całkowita Sprzedaż (PLN)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})
output$sales_location_plot <- renderPlot({
  data <- sales_by_location()
  
  if (is.null(data) || nrow(data) == 0) {
    plot.new()
    text(0.5, 0.5, "Brak danych do wyświetlenia.", cex = 1.5)
    return()
  }
  
  ggplot(data, aes(x = reorder(miejsce_zakupu, -total_sales), y = total_sales)) +
    geom_col(fill = "orange") +  
    labs(
      title = "Sprzedaż na Miejsca",
      x = "Miejsce Zakupu",
      y = "Całkowita Sprzedaż (PLN)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })  
}
