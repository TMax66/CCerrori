shinyServer(function(input, output, session) {

  
  #definizione delle variabili
  
  ds_reac<-reactive({
    ds[which(ds$lab==input$lab),] 
  })

  orig<-reactive({
    dx %>% 
      filter(dx$lab==input$lab) %>% 
      dplyr::select("N.conf"=nconf, "data conf"=dtconf,
                    "data reg"=dtreg, "utente"=utente, "N.istanze rdp"=nrdp,
                    settore,tipo,finalita,materiale,matrice,"sezione"=lab,
                    "laboratorio"=labanalisi)
  })
  
  
  media<-reactive({
    mean(ds_reac()$err, na.rm=T)
    })
  
  xul<-reactive({
    media()+sd(ds_reac()$err, na.rm=T)
    })
  
  xil<-0
  
  xends<-reactive({
    max(ds_reac()$week, na.rm=TRUE)
    })
  
  #definizione output
  output$cc<-renderPlot(
    
    ggplot(data=ds_reac(),aes(x=week, y=err)) + geom_point(size=0.3)+ geom_line(linetype=1, size=0.2)+
    geom_segment(aes(x=0,xend=xends(),y=media(),yend=media()), color="blue", linetype=1,size=0.2)+
    geom_segment(aes(x=0,xend=xends(),y=xul(),yend=xul()), color="blue", linetype=1,size=0.2)+
    geom_segment(aes(x=0,xend=xends(),y=xil,yend=xil), color="blue", linetype=1,size=0.2)+
    facet_grid(settore~tipo)+
    ylab("n.errori")+xlab("settimana")
    
  )
  
  
  
  output$tab<-renderDataTable(ds_reac(),server= FALSE,rownames=FALSE,extensions = 'Buttons',class = 'cell-border stripe',
                              options = list(dom = 'Bfrtip',searching = FALSE,paging = TRUE,autoWidth = TRUE,
                                             pageLength = 10,buttons = c("csv",'excel')))

  output$or<-renderDataTable(orig(),server= FALSE,rownames=FALSE,extensions = 'Buttons',class = 'cell-border stripe',
                               options = list(dom = 'Bfrtip',searching = FALSE,paging = TRUE,autoWidth = TRUE,
                                              pageLength = 10,buttons = c("csv",'excel')))
  
  
  
  
  })
