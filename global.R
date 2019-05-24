library(shiny)
library(datasets)
library(qicharts2)
library(ggplot2)
library(plotly)
library(googlesheets)
library(dplyr)
library(lubridate)
library(DT)


dati<-gs_title("rdp2018")
bg <-gs_read(dati, ws="BG")
so <-gs_read(dati, ws="SO")
va <-gs_read(dati, ws="VA")

dx<-rbind(bg, so, va)
dx$lab<-ifelse(substr(dx$labanalisi, 12,18)=="Bergamo", "BG", 
               ifelse(substr(dx$labanalisi,12,18)=="Sondrio","SO",
                      ifelse(substr(dx$labanalisi,12,17)=="Binago","VA","altrilab")))
dx$dup<-ifelse(duplicated(dx$nconf)==FALSE, "hold", "discard")
dx<-subset(dx, is.na(dx$dtinvio))
ds<-dx
ds<-ds %>% 
  select(nconf,nrdp,settore,tipo,lab,dtreg,dup,dtinvio) %>% 
  mutate("n.errori"=nrdp-1) %>% 
  mutate("ERR"=ifelse(n.errori==0, 0, 1))%>% 
  mutate(dtreg=mdy(dtreg)) %>% 
  mutate("week"=week(dtreg)) %>% 
  filter(lab %in% c("BG", "SO","VA") & dup=="hold" & settore %in% c('Alimenti Uomo','Sanità Animale')) %>% 
  group_by(lab,settore, tipo,week) %>% 
  summarise("err"=sum(ERR),
            "nconf"=n(),
            "%err"=round(mean(ERR)*100,2)) %>% 
            #"sommaerr"=sum(ERR),
            #"varerr"=var(ERR))%>% 
  mutate(err=ifelse(is.nan(err), 0, err))

