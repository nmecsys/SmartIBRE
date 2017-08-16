setwd("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\smart_IBRE\\Banco_central")

library("RDS")

bacen=readRDS("listas_bacen.rds")


library("RSelenium")
library(rvest)

###### 1)dando start no RSelenium ###########################

# checkForServer()
# startServer()

remDr <- remoteDriver(browserName = "chrome")

remDr$open()

Sys.sleep(5)

remDr$navigate("http://www4.bcb.gov.br/pec/series/port/aviso.asps")

Sys.sleep(2)

remDr$navigate("https://www3.bcb.gov.br/sgspub/")

remDr$switchToFrame(Id = "iCorpo")
#Sys.sleep(2)

remDr$executeScript(bacen[1,2])

Sys.sleep(2)

remDr$executeScript(bacen[1,4])

Sys.sleep(2)


pagina<-remDr$getPageSource()
pagina_html<-read_html(pagina[[1]])

tabela_pagina<-html_table(pagina_html,fill = TRUE)
df_series<-data.frame(tabela_pagina[[3]])
names(df_series)<-(iconv(names(df_series),"UTF8","latin1"))
df_series[,4]<-iconv(df_series[,4],"UTF8","latin1")
df_series[,3]<-iconv(df_series[,3],"UTF8","latin1")
tab_series<-df_series[,-c(1,10)]
tab_series$grupo<-bacen[1,1]
tab_series$subgrupo<-bacen[1,3]


for(i in 496:dim(bacen)[1]){
  print(i)
  
  remDr$open()
  
  Sys.sleep(5)
  
  remDr$navigate("http://www4.bcb.gov.br/pec/series/port/aviso.asps")
  
  Sys.sleep(2)
  
  remDr$navigate("https://www3.bcb.gov.br/sgspub/")
  
  remDr$switchToFrame(Id = "iCorpo")
  Sys.sleep(2)
  
  remDr$executeScript(bacen[i,2])
  
  Sys.sleep(2)
  
  remDr$executeScript(bacen[i,4])
  
  Sys.sleep(2)
  pagina<-remDr$getPageSource()
  pagina_html<-read_html(pagina[[1]])
  
  tabela_pagina<-html_table(pagina_html,fill = TRUE)
  
  if(length(tabela_pagina)>2){
    
    if(names(tabela_pagina[[3]])[1]=="X1"){
      df_series<-data.frame(tabela_pagina[[4]])
      if(length(tabela_pagina)>4){
        paginas<-iconv(tabela_pagina[[length(tabela_pagina)]], from = "utf8", to = "latin1")
        paginas<-gsub('Próximo','',paginas)
        paginas<-gsub('Último','',paginas)
        paginas<-gsub('Primeiro','',paginas)
        paginas<-gsub('Anterior','',paginas)
        paginas<-gsub(' |  | ','',paginas)
        
        num_paginas<-substr(paginas, 7, nchar(paginas)-6)
        num<-as.numeric(unlist(as.list(strsplit(num_paginas, ",")[[1]])))
        
        for(j in 2:length(num)){
          j=2
          remDr$executeScript(paste0("javascript:getPagina(",as.character(num[j]),")"))
          pagina<-remDr$getPageSource()
          pagina_html<-read_html(pagina[[1]])
          tabela_pagina<-html_table(pagina_html,fill = TRUE)
          df_series2<-data.frame(tabela_pagina[[4]])
          df_series<-rbind(df_series,df_series2)
        }}
      
    }else{
      df_series<-data.frame(tabela_pagina[[3]]) 
    }
    names(df_series)<-(iconv(names(df_series),"UTF8","latin1"))
    df_series[,4]<-iconv(df_series[,4],"UTF8","latin1")
    df_series[,3]<-iconv(df_series[,3],"UTF8","latin1")
    auxi_tab_series<-df_series[,-c(1,10)]
    auxi_tab_series$grupo<-bacen[i,1]
    auxi_tab_series$subgrupo<-bacen[i,3]
    
    tab_series<-rbind(tab_series,auxi_tab_series)
  }
  
  remDr$close()
  
  Sys.sleep(5)

}

saveRDS(tab_series,"series_tabela.rds")



