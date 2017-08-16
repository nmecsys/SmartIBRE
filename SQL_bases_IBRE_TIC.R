library(RODBC)
library(DBI)
library(zoo)
library(TS)

setwd("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\smart_IBRE\\data/")

dados<-read.csv2("fgv_informação.csv")

inicio=Sys.time()
dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
FGV_base<-sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', '' ,'BASE_IPA'")
odbcCloseAll()


# FGV_base <-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA '1179753',","'20020100',","'20150400' "))
# serie<-ts(FGV_base[,3],start = c(2002,1), freq = 12)
# nome_serie<-FGV_base[,4]


for (i in dados[c(701:800),1]){

a<-paste0("'",as.character(i),"'")
FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA ",a,",'20020100'",",'20141200' "))
serie<-cbind(serie,ts(FGV_base[,3],start = c(2002,1), freq = 12))
nome_serie<-cbind(nome_serie,FGV_base[,4])
}
odbcCloseAll()
fim=Sys.time()

fim-inicio

colnames(serie)=paste0("ST_",unique(nome_serie))

str(serie)
