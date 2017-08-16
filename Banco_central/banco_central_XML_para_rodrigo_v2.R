library(SSOAP)
library(XML)
library(RCurl)
library(XML2R)
library(zoo)
library(RDS)
getSeries <- function(codigos, data.ini = "31/12/1990", remove.old = TRUE){
  
  base<-readRDS("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\smart_IBRE\\Banco_central/series_tabela.rds")
  base_esp<-base[which(base$Esp.=="S"),]
  codigos_especiais<-as.numeric(base_esp[base_esp$Cód.%in% codigos,"Cód."])
  fim = Sys.Date()
  data.fim=paste0(substr(fim,9,10),"/",substr(fim,6,7),"/",substr(fim,1,4))
  #df_periodo<-data.frame(periodicidade=c("M","A","T"),periodo=c(12,1,4))
  
  wsdl <- getURL("https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl")
  doc  <- xmlInternalTreeParse(wsdl)
  
  def <- processWSDL(doc)
  ff  <- genSOAPClientInterface(def = def)
  
  
  if(length(codigos_especiais)>0){
    xmlstr <-ff@functions$getValoresSeriesXML(codigos_especiais, data.ini, data.fim,.opts = list(ssl.verifypeer =FALSE))
    
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
    
    if(length(codigos[-which(codigos==codigos_especiais)])>0){
      xmlstr <-ff@functions$getValoresSeriesXML(codigos[-which(codigos==codigos_especiais)], data.ini, data.fim,.opts = list(ssl.verifypeer =FALSE))

      doc <- xmlInternalTreeParse(xmlstr)

      cleanup <- xpathApply(doc,"//SERIE", function(s) {
        id <- xmlGetAttr(s, "ID")
        s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
        s1 <- t(s1)
        dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
        df2 <- as.data.frame(s1, stringsAsFactors=FALSE)
        df2$SERIE <- id
        df2
      })
      df2 <- Reduce(rbind, cleanup)
      df2$DATAFIM<-df2$DATA
      df2<-df2[c("DATA","DATAFIM","VALOR","BLOQUEADO","SERIE")]

      df<-rbind(df,df2)

      df<-df[,-4]}
    
    }else{
    xmlstr <-ff@functions$getValoresSeriesXML(codigos, data.ini, data.fim,.opts = list(ssl.verifypeer =FALSE))
    
    doc <- xmlInternalTreeParse(xmlstr)
    
    cleanup <- xpathApply(doc,"//SERIE", function(s) {
      id <- xmlGetAttr(s, "ID")
      s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
      s1 <- t(s1)
      dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
      df<- as.data.frame(s1, stringsAsFactors=FALSE)
      df$SERIE <- id
      df
    })
    
    df <- Reduce(rbind, cleanup)
    df$data  <- as.Date(sapply(strsplit(df$DATA,  "/"),
                               function(x) paste(c(x[2:1], 1), collapse="-")), "%Y-%m-%d")
    
    df$valor <- as.numeric(df$VALOR)
    df$serie <- factor(df$SERIE)
    
    # if(remove.old){
    #   df$BLOQUEADO <- NULL
    #   df$SERIE <- NULL
    #   df$DATA <- NULL
    #   df$VALOR <- NULL
    # }
    # 
    
    #start_mes<-1
    if(is.na(df$data[1])==TRUE || grepl("0001",df$data[1])==TRUE){
      df$data<-df$DATA
      df$valor<-df$VALOR}
    
    df<-df[,-c(1:4)]}
  
  df}


