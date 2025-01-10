dzialania_tab <- tabItem(
  tabName = "dzialania",
  h2("Rekomendowane działania"),
  br(),
  div(
    style = "overflow: scroll;",
    uiOutput("rekomendacje")

  )
  
)
