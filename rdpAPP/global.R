library(shiny)
library(datasets)
library(qicharts2)
library(ggplot2)
library(plotly)
library(googlesheets)
library(dplyr)
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
ds<-ds %>% 
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
