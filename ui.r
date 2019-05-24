ui <- fluidPage(
  titlePanel("Errori di registrazione: Carte di controllo"),
  tabsetPanel(type="tabs",
              tabPanel("carta di controllo", 
  # titlePanel("Carte di controllo errori di registrazione"),
  sidebarPanel(
    selectInput("lab","Sezione",
                c(unique(ds$lab)),selected = "")
  ),
  mainPanel(
    plotOutput("cc"),
    hr(),
    br(),
    DT::dataTableOutput("tab")
  )
),
tabPanel("Origine dati",
         mainPanel(
           DT::dataTableOutput("or")
         ))
)
)

