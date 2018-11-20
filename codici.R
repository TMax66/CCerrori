library(googlesheets)
library(tidyverse)
library(lubridate)

rm(list=ls())
dati<-gs_title("rdp2018")
bg <-gs_read(dati, ws="BG")
so <-gs_read(dati, ws="SO")
va <-gs_read(dati, ws="VA")

ds<-rbind(bg, so, va)


ds$lab<-ifelse(substr(ds$labanalisi, 12,18)=="Bergamo", "BG", 
               ifelse(substr(ds$labanalisi,12,18)=="Sondrio","SO",
                      ifelse(substr(ds$labanalisi,12,17)=="Binago","VA","altrilab")))

ds$dup<-ifelse(duplicated(ds$nconf)==FALSE, "hold", "discard")
ds<-subset(ds, is.na(ds$dtinvio))


dx<-ds %>% 
  select(nconf,nrdp,settore,tipo,lab,dtreg,dup,dtinvio) %>% 
  mutate("n.errori"=nrdp-1) %>% 
  mutate("ERR"=ifelse(n.errori==0, 0, 1))%>% 
  mutate(dtreg=mdy(dtreg)) %>% 
  mutate("week"=week(dtreg)) %>% 
  filter(lab %in% c("BG", "SO","VA") & dup=="hold" & settore %in% c('Alimenti Uomo','Sanità Animale')) %>% 
  group_by(lab,settore, tipo,week) %>% 
  summarise("y"=sum(ERR),
            "nconf"=n(),
            "yx"=mean(ERR),
            "sommaerr"=sum(ERR),
            "varerr"=var(ERR))%>% 
  mutate(y=ifelse(is.nan(y), 0, y))
  media<-mean(dx$y, na.rm=T)
  xul<-media+sd(dx$y, na.rm=T)
  xil<-0
  xends<-max(dx$week, na.rm=TRUE)
  ggplot(data=dx,aes(x=week, y= y)) + geom_point(size=0.3)+geom_line(linetype=1, size=0.2)+
  geom_segment(aes(x=0,xend=xends,y=media,yend=media), color='blue', linetype=1,size=0.2)+
  geom_segment(aes(x=0,xend=xends,y=xul,yend=xul), color='blue', linetype=1,size=0.2)+
  geom_segment(aes(x=0,xend=xends,y=xil,yend=xil), color='blue', linetype=1,size=0.2)+
    facet_grid(lab~settore+tipo)+
    ylab("n.errori")+xlab("settimana")
  
  

             
