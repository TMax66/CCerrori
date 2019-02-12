ui <- navbarPage("Carte di controllo errori di registrazione",
                 
                 tabPanel("",
                          fluidPage(
                            sidebarPanel(
                              selectInput("lab","Sezione",
                                    c(unique(as.character(ds$lab))))
                          ),
                          
                 mainPanel(
                   plotOutput("cc")
                 )     
)))