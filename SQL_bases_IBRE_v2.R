library(RODBC)
library(DBI)
library(zoo)
library(TS)

setwd("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\smart_IBRE\\data/")

dados<-read.csv2("fgv_informação.csv")

inicio=Sys.time()
dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
# FGV_base<-sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', '' ,'base_ICC'")
# Sys.sleep(15)
FGV_base<-sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '19440100','20160600', '' ,'base_ICC'")
Sys.sleep(15)
FGV_base<-rbind(FGV_base,sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '19440100','20160600', '' ,'base_IGP'"))
Sys.sleep(15)
FGV_base<-rbind(FGV_base,sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '19440100','20160600', '' ,'base_IPA'"))
odbcCloseAll()
Sys.sleep(15)
odbcCloseAll()

dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
FGV_base2<-sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', '' ,'base_IPC'")
Sys.sleep(15)
FGV_base2<-rbind(FGV_base2,sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', '' ,'SIA2'"))
odbcCloseAll()


Sys.sleep(15)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
FGV_base3<-sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', '' ,'base_SONDA'")
Sys.sleep(15)
FGV_base3<-rbind(FGV_base3,sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', '' ,'FGV-EXPECT'"))
Sys.sleep(15)
FGV_base3<-rbind(FGV_base3,sqlQuery(dbhandle,"exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', '' ,'FGV-SONDA'"))
odbcCloseAll()

setwd("C:\\Users\\anna.barros\\Dropbox\\testes_sql")

base1<-read.csv2("base1.csv")
base2<-read.csv2("base2.csv")
base3<-read.csv2("base3.csv")

head(FGV_base3)

teste3<-base3$Codigo %in% FGV_base3$CD_SERIE
teste2<-base2$Código %in% FGV_base2$CD_SERIE
teste1<-base1$Código %in% FGV_base$CD_SERIE
FALSE %in% teste3

codigos<-base3[which(teste3==FALSE),]

base<-subset(FGV_base,FGV_base$CD_SERIE==unique(FGV_base$CD_SERIE[1]))
frame<-data.frame(base[,c(1,3)])

for(i in 1:length(unique(FGV_base$CD_SERIE))){

  base<-subset(FGV_base,FGV_base$CD_SERIE==unique(FGV_base$CD_SERIE)[i])
  
  if(dim(base)[1]<dim(frame)[1]){
    num<-(dim(frame)[1]-dim(base)[1])
    base_base<-rep(NA,num)
    encaixe<-c(base_base,base[,3])
    frame<-cbind(frame,encaixe)
    
  }else{
    frame<-cbind(frame,base[,3])
    
  }
 
}


names(frame)<-c("Data",paste0("ST_",as.character(unique(FGV_base$CD_SERIE))))


















