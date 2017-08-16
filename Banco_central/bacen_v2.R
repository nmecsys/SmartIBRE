library("RSelenium")
checkForServer()
startServer()

#Iniciando uma conex?o
#fprof <- makeFirefoxProfile(list(browser.download.dir = "C:\\Users\\anna.barros\\Dropbox"))
remDr <- remoteDriver(browserName = "chrome")
remDr$open()

Sys.sleep(5)

remDr$navigate("http://www4.bcb.gov.br/pec/series/port/aviso.asps")

Sys.sleep(2)

remDr$navigate("https://www3.bcb.gov.br/sgspub/")

remDr$switchToFrame(Id = "iCorpo")
#remDr$switchToFrame(Id = "iCorpo")

links <- remDr$findElements(using = 'css selector', "a")
linkHref <- sapply(links, function(x) x$getElementAttribute('href'))
linkHref
linkHref[[3]][[1]]

Sys.sleep(5)


#remDr$phantomExecute("javascript:parent.montarArvoreGrupos('localizarSeries.do?method=prepararTelaLcsArvore',1022,3,false);")
remDr$executeScript(linkHref[[3]][[1]])
Sys.sleep(5)


iconv(URLdecode("Com%C3%A9rcio%20"),"UTF-8","latin1")
remDr$executeScript("javascript:selecionarGrupoLink(1164, 'Comércio exterior');")

print("PASSEI")
#remDr$getPageSource()

Sys.sleep(3)

Sys.sleep(1)
#remDr$executeScript("javascript:selecionarGrupoLink(1234, 'Crédito');")
Sys.sleep(1)

webel <- remDr$findElement("name", "cbxSelecionaSerie")

Sys.sleep(1)
webel$clickElement()
Sys.sleep(1)
webel$isElementSelected()
Sys.sleep(1)
#remDr$phantomExecute("javascript:acaoBotaoVoltar();")
#remDr$executeScript("javascript:acaoBotaoVoltar();")

#webel$get


remDr$switchToFrame(NULL)
Sys.sleep(1)
minhatag <- remDr$findElement(using = "class name", "botao")
remDr$executeScript("javascript:acaoBotaoVoltar();")
Sys.sleep(10)
remDr$executeScript("javascript:consultarValores(document.forms[0]);")
Sys.sleep(5)
links2 <- remDr$findElements(using = 'css selector', "a")
linkHref2<- sapply(links2, function(x) x$getElementAttribute('href'))


remDr$navigate("https://www3.bcb.gov.br/sgspub/consultarvalores/consultarValoresSeries.do?method=downLoad")



# mybrowser$navigate("https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries")
# 
# mybrowser$switchToFrame(Id = "iCorpo")
# 
# final <- mybrowser$findElement(using = "class name", "naoimprime")
# checar <- final$findChildElement(using = "xpath", "*")
# 
# 
# htmlParse(checar)
# checar


set.tempdir <- function(path) {
  invisible(.Call(C_setTempDir, path.expand(path)))
}

set.tempdir("C:\\Users\\anna.barros\\Dropbox\\bacen")
