usunModuleServer <- function(id, input, output, session, con) {
  selected_to_delete <- reactiveValues(ids = c())
  output$tabelaProduktyUsun <- renderDT({
    produkty <- dbGetQuery(con, "SELECT id_produktu, nazwa_produktu, cena_sprzedazy, ilosc_magazyn FROM produkty")
    
    datatable(
      produkty,
      selection = 'none',  
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            targets = 0,  
            visible = FALSE 
          ),
          list(
            targets = 1,
            render = JS(
              "function(data, type, row, meta) {",
              "return '<input type=\"checkbox\" class=\"delete-checkbox\" data-id=\"' + row[0] + '\" /> ' + data;",
              "}"
            )
          )
        )
      ),
      callback = JS("
        // Nasłuchiwanie na kliknięcie wiersza
        table.on('click', 'tbody tr', function(){
          var data = table.row(this).data();
          var id = data[0];  // Zakładamy, że pierwsza kolumna to id_produktu
          Shiny.setInputValue('usun_selected_row', id, {priority: 'event'});
        });
        
        // Nasłuchiwanie na zmianę stanu checkboxów
        table.on('change', 'input.delete-checkbox', function(){
          var id = $(this).data('id');
          var isChecked = this.checked;
          Shiny.setInputValue('selected_ids', {id: id, checked: isChecked}, {priority: 'event'});
        });
      ")
    )
  }, server = FALSE)
  
  observeEvent(input$selected_ids, {
    req(input$selected_ids)
    id <- input$selected_ids$id
    checked <- input$selected_ids$checked
    
    if (checked) {
      selected_to_delete$ids <- unique(c(selected_to_delete$ids, id))
    } else {
      selected_to_delete$ids <- setdiff(selected_to_delete$ids, id)
    }
  })
  
  observeEvent(input$usun_wybrane, {
    req(selected_to_delete$ids)
    
    showModal(modalDialog(
      title = "Potwierdzenie Usunięcia",
      paste("Czy na pewno chcesz usunąć", length(selected_to_delete$ids), "produktów?"),
      footer = tagList(
        modalButton("Anuluj"),
        actionButton("confirm_usun", "Usuń", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_usun, {
    removeModal()
    
    ids <- selected_to_delete$ids
    if (length(ids) > 0) {
      placeholders <- paste(rep("?", length(ids)), collapse = ",")
      query <- sprintf("DELETE FROM produkty WHERE id_produktu IN (%s)", placeholders)
      
      tryCatch({
        dbExecute(con, query, params = as.list(ids))
        showNotification(paste("Usunięto", length(ids), "produktów."), type = "message")
        selected_to_delete$ids <- c()
        
        output$tabelaProduktyUsun <- renderDT({
          produkty <- dbGetQuery(con, "SELECT id_produktu, nazwa_produktu, cena_sprzedazy, ilosc_magazyn FROM produkty")
          
          datatable(
            produkty,
            selection = 'none',  
            escape = FALSE,
            rownames = FALSE,
            options = list(
              pageLength = 10,
              scrollY = "300px",
              scrollCollapse = TRUE,
              autoWidth = TRUE,
              columnDefs = list(
                list(
                  targets = 0, 
                  visible = FALSE  
                ),
                list(
                  targets = 1,  
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return '<input type=\"checkbox\" class=\"delete-checkbox\" data-id=\"' + row[0] + '\" /> ' + data;",
                    "}"
                  )
                )
              )
            ),
            callback = JS("
              table.on('click', 'tbody tr', function(){
                var data = table.row(this).data();
                var id = data[0];  
                Shiny.setInputValue('usun_selected_row', id, {priority: 'event'});
              });
              
              table.on('change', 'input.delete-checkbox', function(){
                var id = $(this).data('id');
                var isChecked = this.checked;
                Shiny.setInputValue('selected_ids', {id: id, checked: isChecked}, {priority: 'event'});
              });
            ")
          )
        }, server = FALSE)
      }, error = function(e) {
        showNotification(paste("Błąd podczas usuwania produktów:", e$message), type = "error")
        print(e)
      })
    }
  })
}

  
  