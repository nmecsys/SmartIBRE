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

links <- remDr$findElements(using = 'css selector', "a")
linkHref <- sapply(links, function(x) x$getElementAttribute('href'))
linkHref
linkHref[[3]][[1]]

remDr$phantomExecute("javascript:parent.montarArvoreGrupos('localizarSeries.do?method=prepararTelaLcsArvore',1,1,false);")
remDr$executeScript("javascript:parent.montarArvoreGrupos('localizarSeries.do?method=prepararTelaLcsArvore',1,1,false);")

Sys.sleep(5)

remDr$executeScript("javascript:selecionarGrupoLink(4, 'Produto Interno Bruto (PIB)');")

Sys.sleep(5)

# webel <- remDr$findElement("name","cbxSelecionaSerie")
# 
# webel
# 
# checkbox <- sapply(webel, function(x) x$getElementAttribute('href'))

#webel <- remDr$findElement("name","cbxSelecionaSerie")
Sys.sleep(1)
#achando_valor<-remDr$findElement("tag name", "td")

pagina<-remDr$getPageSource()
pagina_html<-read_html(pagina[[1]])

tabela_pagina<-html_table(pagina_html,fill = TRUE)
df_series<-data.frame(tabela_pagina[[3]])
names(df_series)<-(iconv(names(df_series),"UTF8","latin1"))
df_series[,4]<-iconv(df_series[,4],"UTF8","latin1")
df_series[,3]<-iconv(df_series[,3],"UTF8","latin1")
tab_series<-df_series[,-c(1,10)]






codigos<-data.frame(tabela_pagina[2])[-c(1,2),2]

webElem <- remDr$findElement(using = 'xpath', "//*/input[@value = 4192]")
Sys.sleep(1)
webElem$clickElement()
Sys.sleep(1)
#webel$isElementSelected()

remDr$switchToFrame(NULL)
Sys.sleep(1)
minhatag <- remDr$findElement(using = "class name", "botao")
remDr$executeScript("javascript:acaoBotaoVoltar();")
Sys.sleep(10)
remDr$executeScript("javascript:consultarValores(document.forms[0]);")
Sys.sleep(5)



carolina<-read_html(remDr$getPageSource()[[1]])

tabela<-html_table(carolina,fill=TRUE)

paginas<-iconv(tabela[[6]][,1], from = "utf8", to = "latin1")

paginas<-gsub('Â','',paginas)
paginas<-gsub(' ','',paginas)
paginas<-gsub('Próximo','',paginas)
paginas<-gsub('Último','',paginas)
paginas<-gsub('Primeiro','',paginas)
paginas<-gsub('Anterior','',paginas)
paginas<-gsub(' |  | ','',paginas)
    
num_paginas<-substr(paginas, 7, nchar(paginas)-6)

num<-length(as.list(strsplit(num_paginas, ",")[[1]]))


a="javascript:getPagina(3)"

remDr$executeScript(a)

Sys.sleep(5)
carolina<-read_html(remDr$getPageSource()[[1]])


tabela<-html_table(carolina,fill=TRUE)


me_int<-tabela[[5]]

me_int[,1]<-iconv(me_int[,1], from = "utf8", to = "latin1")
me_int[,2]<-iconv(me_int[,2], from = "utf8", to = "latin1")

