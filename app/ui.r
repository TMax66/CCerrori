ui <- fluidPage(
  titlePanel("Carte di controllo errori di registrazione"),
  sidebarPanel(
    selectInput("lab","Sezione",
                c(unique(as.character(ds$lab))))
  ),
  mainPanel(
    plotOutput("cc")
  )
)

