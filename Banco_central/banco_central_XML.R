library(SSOAP)
library(XML)
library(RCurl)
library(XML2R)
library(zoo)

getSeries <- function(codigos, data.ini = "31/12/1962", remove.old = FALSE,periodicidade="M"){
  
  codigos=c(1207,1211)

  df_periodo<-data.frame(periodicidade=c("M","A","T"),periodo=c(12,1,4))
  
  fim = Sys.Date()
  
  data.fim=paste0(substr(fim,9,10),"/",substr(fim,6,7),"/",substr(fim,1,4))
  

  wsdl <- getURL("https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl")
  doc  <- xmlInternalTreeParse(wsdl)
  
  def <- processWSDL(doc)
  ff  <- genSOAPClientInterface(def = def)
  
  
  xmlstr <- ff@functions$getValoresSeriesXML(codigos, data.ini, data.fim, 
                                             .opts = list(ssl.verifypeer = FALSE))
  doc <- xmlInternalTreeParse(xmlstr)
  
  cleanup <- xpathApply(doc,"//SERIE", function(s) {
    id <- xmlGetAttr(s, "ID")
    s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
    s1 <- t(s1)
    dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
    df <- as.data.frame(s1, stringsAsFactors=FALSE)
    df$SERIE <- id
    df
  })
  df <- Reduce(rbind, cleanup)
  
  df$data  <- as.Date(sapply(strsplit(df$DATA,  "/"),
                             function(x) paste(c(x[2:1], 1), collapse="-")), "%Y-%m-%d")
  df$valor <- as.numeric(df$VALOR)
  df$serie <- factor(df$SERIE)
  
  if(remove.old){
    df$BLOQUEADO <- NULL
    df$SERIE <- NULL
    df$DATA <- NULL
    df$VALOR <- NULL
  }
  
  start_ano<-as.numeric(substr(df[1,1],1,4))
  start_mes<-as.numeric(substr(df[1,1],6,7))
  
  if(periodicidade=="M"){
    if(length(unique(df[,3]))==1){
      start_ano<-as.numeric(substr(df[1,1],1,4))
      start_mes<-as.numeric(substr(df[1,1],6,7))
      
      minha_ts<-ts(df[,2],start<-c(start_ano,start_mes),freq=df_periodo[df_periodo$periodicidade==periodicidade,2])
      
    }else{
      
      cods<-unique(df[,3])
      df_cortado<-df[df$serie==cods[1],]
      start_ano<-as.numeric(substr(df_cortado[1,1],1,4))
      start_mes<-as.numeric(substr(df_cortado[1,1],6,7))
      minha_ts<-ts(df_cortado[,2],start<-c(start_ano,start_mes),freq=df_periodo[df_periodo$periodicidade==periodicidade,2],names = 1555)
      
      for(i in 2:length(cods)){
        
        df_cortado<-df[df$serie==cods[i],]
        start_ano<-as.numeric(substr(df_cortado[1,1],1,4))
        start_mes<-as.numeric(substr(df_cortado[1,1],6,7))
        minha_ts<-cbind(minha_ts,ts(df_cortado[,2],start<-c(start_ano,start_mes),freq=df_periodo[df_periodo$periodicidade==periodicidade,2]))
      }}
    }else if(periodicidade=="A"){
      if(length(unique(df[,4]))==1){
      start_ano<-as.numeric(substr(df[1,1],1,4))
      start_mes<-as.numeric(substr(df[1,1],6,7))
      
      minha_ts<-ts(df[,2],start<-start_ano,freq=df_periodo[df_periodo$periodicidade==periodicidade,2])
      
    }else{
      
      cods<-unique(df[,4])
      df_cortado<-df[df$serie==cods[1],]
      start_ano<-as.numeric(substr(df_cortado[1,1],1,4))
      start_mes<-as.numeric(substr(df_cortado[1,1],6,7))
      minha_ts<-ts(df_cortado[,2],start<-start_ano,freq=df_periodo[df_periodo$periodicidade==periodicidade,2])
      
      for(i in 2:length(cods)){
        
        df_cortado<-df[df$serie==cods[i],]
        start_ano<-as.numeric(substr(df_cortado[1,1],1,4))
        start_mes<-as.numeric(substr(df_cortado[1,1],6,7))
        minha_ts<-cbind(minha_ts,ts(df_cortado[,2],start<-start_ano,freq=df_periodo[df_periodo$periodicidade==periodicidade,2]))
      }}}
  
  lista<-list(ts<-minha_ts,data<-as.Date(minha_ts))
  }


