setwd("V://SUEP//Núcleo de Métodos Estatísticos e Computacionais//19 Shiny//smart_IBRE//Banco_central")
library(zoo)

library(RDS)

base<-readRDS("series_tabela.rds")

base[,3]

getwd()

source("banco_central_XML.R")

beta<-getSeries(1554)

time_serie<-ts(alfa[,2],start = c(as.numeric(substr(alfa[1,1],1,4)),as.numeric(substr(alfa[1,1],6,7))),freq=12)

names(time_serie)<-"serie"

time_serie

########## baixando da base de series 

codigo<-base[c(5,7,2000),1]


<-getSeries(codigo)



