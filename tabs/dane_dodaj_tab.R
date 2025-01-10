library(shinydashboard)
library(DT)

dane_dodaj_tab <- tabItem(
  tabName = "dodaj",
  fluidRow(
    column(
      width = 6,
      box(
        title = "Dodaj nowy produkt",
        status = "success",
        solidHeader = TRUE,
        width = NULL,
        textInput("nazwa_produktu", "Nazwa produktu", ""),
        selectInput("kategoria", "Kategoria", choices = NULL),
        numericInput("cena_zakupu", "Cena zakupu (zł)", value = 0, min = 0),
        numericInput("cena_sprzedazy", "Cena sprzedaży (zł)", value = 0, min = 0),
        textInput("kolor", "Kolor", ""),
        selectInput("producent", "Producent", choices = NULL),
        textInput("kod_towaru", "Kod towaru (SKU)", ""),
        textAreaInput("opis", "Opis", "", rows = 3),
        numericInput("ilosc_magazyn", "Ilość na magazynie", value = 0, min = 0),
        dateInput( inputId = "data_zamowienia", label = "Data zamówienia", value = as.Date("2024-12-27"), format = "dd.mm.yyyy", language= "pl"),
        actionButton("dodaj_produkt", "Dodaj Produkt", class = "btn btn-primary")
      ),
      box(
        title = "Import",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        downloadButton("download_template", "Pobierz szablon Excel"),
        br(), br(),
        fileInput("upload_csv", "Wczytaj plik CSV", accept = c(".xlsx"))
      )
    ),
    
    column(
      width = 6,
      box(
        title = "Podgląd produktów",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        div(
          style = "max-height: 500px; overflow-y: auto;",
          DTOutput("tabelaProduktyView")
        )
      )
    )
  )
)
