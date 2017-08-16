library("RSelenium")
checkForServer()
startServer()

#Iniciando uma conex?o
mybrowser <- remoteDriver()
mybrowser$open()


mybrowser$navigate("https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries")

mybrowser$switchToFrame(Id = "iCorpo")

final <- mybrowser$findElement(using = "class name", "naoimprime")
checar <- final$findChildElement(using = "xpath", "*")


htmlParse(checar)
checar

devtools::install_github("ropensci/RSelenium")
install.packages("Rtools")

remove.packages(RCurl)
