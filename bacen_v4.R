library("RSelenium")
library(rvest)

###### 1)dando start no RSelenium ###########################

checkForServer()
startServer()

remDr <- remoteDriver(browserName = "firefox")

remDr$open()

Sys.sleep(5)

remDr$navigate("http://www4.bcb.gov.br/pec/series/port/aviso.asps")

Sys.sleep(2)

remDr$navigate("https://www3.bcb.gov.br/sgspub/")

remDr$switchToFrame(Id = "iCorpo")

GRUPOS<-read_html(remDr$getPageSource()[[1]])


tabela_grupos<-html_table(GRUPOS,fill=TRUE)[[1]]

tabela_grupos[,1]<-iconv(tabela_grupos[,1],"UTF-8","latin1")
tabela_grupos[,2]<-iconv(tabela_grupos[,2],"UTF-8","latin1")

grupos<-vector(length=2*nrow(tabela_grupos), mode='character')

grupos[seq(from = 1, to = 2*dim(tabela_grupos)[1], by=2)]<-tabela_grupos[,1]
grupos[seq(from = 2, to = 2*dim(tabela_grupos)[1], by=2)]<-tabela_grupos[,2]

links <- remDr$findElements(using = 'css selector', "a")
linkHref <- sapply(links, function(x) x$getElementAttribute('href'))
# remDr$phantomExecute("javascript:parent.montarArvoreGrupos('localizarSeries.do?method=prepararTelaLcsArvore',1,1,false);")
# remDr$executeScript("javascript:parent.montarArvoreGrupos('localizarSeries.do?method=prepararTelaLcsArvore',1,1,false);")
# 
# galhos_links<- remDr$findElements(using = 'css selector', "a")
# galhos_puros <- sapply(galhos_links, function(x) x$getElementAttribute('href'))

tabela_grupos_link<-data.frame(grupos=grupos,javascript=unlist(linkHref)[c(1:length(grupos))])


# arvore_principal<-linkHref[[1]][[1]]
# 
# for (i in 2:length(linkHref)){
#   
#   arvore_principal<-cbind(arvore_principal,linkHref[[i]][[1]])
#   
# }

arvores_usada<-tabela_grupos_link[grepl("javascript:parent", tabela_grupos_link[,2])== TRUE,]
arvores_usada[,1]<-as.character(arvores_usada[,1])
arvores_usada[,2]<-as.character(arvores_usada[,2])

tabela_enderecos<-data.frame(grupos=character(),javascript_grupo=character(),subgrupo=character(),javascript_subgrupo=character())


for (link in c(1,2,3,4,5,6,7,8,10,11,12)){

  remDr$open()
  remDr$navigate("http://www4.bcb.gov.br/pec/series/port/aviso.asps")
  Sys.sleep(5)
  
  remDr$navigate("https://www3.bcb.gov.br/sgspub/")
  
  remDr$switchToFrame(Id = "iCorpo")
  Sys.sleep(5)
  
  try(remDr$phantomExecute(arvores_usada[link,2]), silent = TRUE)
  remDr$executeScript(arvores_usada[link,2])
  
  Sys.sleep(5)
  
  galhos_links2<- remDr$findElements(using = 'css selector', "a")
  galhos_puros2 <- t(as.data.frame(sapply(galhos_links2, function(x) x$getElementAttribute('href'),simplify = "matrix")))
  galhos_interessantes<-galhos_puros2[grepl("Camada", galhos_puros2[,1])== FALSE & grepl("javascript:", galhos_puros2[,1])==TRUE & 
                                        grepl("\\<go\\>", galhos_puros2[,1])==FALSE & grepl("print", galhos_puros2[,1])==FALSE ]
  
  
  galhos_interessantes<-gsub("\\s*\\([^\\)]+%\\)","()",galhos_interessantes)
  galhos_interessantes<-iconv(sapply(galhos_interessantes,URLdecode,USE.NAMES = FALSE),"UTF-8","latin1")
  
  lista_galhos<-substr(galhos_interessantes[1],gregexpr(pattern =",",galhos_interessantes[1])[[1]][1]+1, 
                       gregexpr(pattern =')',galhos_interessantes[1])[[1]][length(gregexpr(pattern =')',galhos_interessantes[1])[[1]])]-1)
  galhos_interessantes<-gsub("\\s*\\([^\\)]+%\\)","(%)",galhos_interessantes)
  for (javas in  2:length(galhos_interessantes)){
    lista_galhos<-cbind(lista_galhos,substr(galhos_interessantes[javas],gregexpr(pattern =",",galhos_interessantes[javas])[[1]][1]+1, 
                                            gregexpr(pattern =')',galhos_interessantes[javas])[[1]][length(gregexpr(pattern =')',galhos_interessantes[javas])[[1]])]-1))
  }
  
  if (link <2){
    table_endereco<-data.frame(t(lista_galhos))
    names(table_endereco)<-"Subgrupos"
    
    table_endereco$javascript_subgrupo<-galhos_interessantes
    
    table_endereco$grupo<-arvores_usada[link,1]
    table_endereco$javascript_grupo<-arvores_usada[link,2]
    
    table_endereco<-table_endereco[,c("grupo","javascript_grupo","Subgrupos","javascript_subgrupo")]
    
  } else {
    
    table_ajuda<-data.frame(t(lista_galhos))
    names(table_ajuda)<-"Subgrupos"
    
    table_ajuda$javascript_subgrupo<-(galhos_interessantes)
    
    table_ajuda$grupo<-arvores_usada[link,1]
    table_ajuda$javascript_grupo<-arvores_usada[link,2]
    
    table_ajuda<-table_ajuda[,c("grupo","javascript_grupo","Subgrupos","javascript_subgrupo")]
    
    table_endereco<-rbind(table_endereco,table_ajuda)
    
  }
  remDr$close()
  
  }

link

IPCA<-subset(table_endereco,table_endereco$Subgrupos==" 'Índices de preços ao consumidor'")
