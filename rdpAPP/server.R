shinyServer(function(input, output, session) {
  
  
  output$cc<-renderPlot(

    ds<-ds %>%
    filter(lab==input$lab),
    media<-mean(ds$y, na.rm=T),
    xul<-media+sd(ds$y, na.rm=T),
    xil<-0,
    xends<-max(ds$week, na.rm=TRUE),

    ggplot(data=ds,aes(x=week, y=y)) + geom_point(size=0.3) #+ #geom_line(linetype=1, size=0.2)+
      # geom_segment(aes(x=0,xend=xends,y=media,yend=media), color="blue", linetype=1,size=0.2)+
      # geom_segment(aes(x=0,xend=xends,y=xul,yend=xul), color="blue", linetype=1,size=0.2)+
      # geom_segment(aes(x=0,xend=xends,y=xil,yend=xil), color="blue", linetype=1,size=0.2)+
      #facet_grid(settore~tipo)+
      #ylab("n.errori")+xlab("settimana")

  )


})