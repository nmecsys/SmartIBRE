# pacotes
library(shiny)
library(BETS)
library(dygraphs)
library(forecast)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(xlsx)
library(foreign)
library(zoo)
library(quantmod)
library(rvest)
library(datasets)
library(glmulti)
library(DT)
library(RColorBrewer)
library(googleVis)
library(WDI)
library(RDS)
library(lubridate)
library(stringr)
library(dplyr)
library(xtable)
library(RODBC)
library(DBI)
#library(stargazer)
library(chron)

 
# Ajuste sazonal ------------------------------------------
Sys.setenv(X13_PATH = "./data")
library(seasonal)
air <- AirPassengers
drive <- UKDriverDeaths
pim <- readRDS("data/pim.rds")
ipc <- readRDS("data/ipc.rds")

# helpers -------------------------------------------------
source("helpers.R")

# séries mundo -----------------------------------------------------------------------
cod_series_mundo <- read.csv2("data/codigos_series_mundo.csv",stringsAsFactors=F)

# índice de gini
gini <- WDI(indicator='SI.POV.GINI', start = 2010, end = 2010)
gini <- na.omit(gini)
colnames(gini) <- c("COD", "País", "Gini", "Ano")

# expectativa de vida ao nascer
expec <- WDI(indicator='SP.DYN.LE00.IN', start = 2012, end = 2012)
expec <- na.omit(expec)
colnames(expec) <- c("COD", "País", "Expectativa", "Ano")

# mortalidade infantil
tmi <- WDI(indicator='SH.DYN.MORT', start = 2010, end = 2010)
tmi <- na.omit(tmi)
colnames(tmi) <- c("COD", "País", "TMI", "Ano")

# população
pop <- WDI(indicator='SP.POP.TOTL', start = 2013, end = 2013)
pop <- na.omit(pop)
colnames(pop) <- c("COD", "País", "População", "Ano")

# séries IBGE -----------------------------------------------------------------------------
# url_IBGE<-read_html("http://seriesestatisticas.ibge.gov.br/lista_tema.aspx?op=2&no=1")
# ibge<-data.frame(html_table(url_IBGE))
# names(ibge)<-iconv(ibge[1,],"UTF-8","latin1")
# ibge<-ibge[-1,]
# ibge[,2]<-iconv(ibge[,2],"UTF-8","latin1")
# 
# dados<-ibge
# #dados <- readRDS("data/IBEG.rds")
# #dados<-read.csv2("data/IBEG_v2.csv")
# dados[,3]<-as.character(dados[,3])
# dados <- subset(dados,dados[,3] %in% c("Trimestral","Mensal","Anual"))
# dados[,1]<-as.character(dados[,1])
# a<-data.frame(do.call('rbind', strsplit(as.character(dados$Período),'-',fixed=TRUE)))[2]
# #dados[,5]<-as.character(dados[,5])
# #dados<-subset(dados,dados[,5]=="Trimestral")
# dados_faltante<- readRDS("data/IBGE_faltante.rds")
# dados_faltante[,1]<-as.character(dados_faltante[,1])
# dados_faltante[,6]<-as.character(dados_faltante[,6])
# names_IBGE<-as.character(list(dados[,2])[[1]])
# codigos<-as.character(list(dados[,1])[[1]])

#setwd("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\smart_IBRE\\Banco_central")
base_IBGE<-readRDS("./Banco_central/series_tabela_v2.rds")
#base_IBGE<-subset(base_bacen,base_bacen$Per.=="M")

base_IBGE<-subset(base_IBGE,grepl("IBGE",base_IBGE$Fonte)==TRUE)
names(base_IBGE)[5]<-"Inicio"
names(base_IBGE)[6]<-"Fim"
names(base_IBGE)[2]<-"Nome"
names(base_IBGE)[4]<-"Periodicidade"
# source("api_ibge.R")

# Séries FGV IBRE -----------------------------------------------------------------
series <- readRDS("data/fgv_indices.rds")
ipc <- series[,2]
# incc <- series[,3]
# ipa <- series[,4]
igp <- readRDS("data/igp.rds")

FGV <- readRDS("data/FGV_dados.rds")
infos <- readRDS("data/fgv_informacao.rds")
infos[,2]<-as.character(infos[,2])
# Nomes das colunas de infos
# 1:Código 2:Nome 3:Unidade 4:Periodicidade 5:Início        
# 6:Fim 7:Base 8:Fonte 9:Serviço 10:Descontinuação 11:Item_fgv
# 12:Cod_estrutura 13:Estrutura 14:Cod_Estagio 15:Estagio 
# 16:Cod_grupo 17:Grupo 18:Cod_subgrupo
# 19:Subgrupo 20:Cod_item 21:Item 22:Cod_subitem 23:Subitem       

# IPA
subset_ipa <- subset(infos, infos$Fonte == "Índices de Preço por Atacado")
ipa_unidades <- c("Variação","Número Índice")
ipa_unidades_ok <- c("PERCENT", "INDICE")
ipa_estruturas <- c("IPA por Estágios", "IPA por Origem")
ipa_estruturas_ok <- c("EP", "OG")

# IPC
ipc_estrutura <- readRDS("data/ipc_estrutura.rds")
subset_ipc <- subset(infos, infos$Fonte == "Índices de Preço ao Consumidor")
subset_ipc[,2]<-as.character(subset_ipc[,2])
subset_ipc[,3]<-as.character(subset_ipc[,3])
subset_ipc[,4]<-as.character(subset_ipc[,4])
subset_ipc[,5]<-as.character(subset_ipc[,5])
subset_ipc[,6]<-as.character(subset_ipc[,6])
subset_ipc[,7]<-as.character(subset_ipc[,7])
subset_ipc[,8]<-as.character(subset_ipc[,8])
subset_ipc[,9]<-as.character(subset_ipc[,9])

ipc_regioes <- c("IPC - Brasil", "IPC - Belo Horizonte", "IPC - Brasília", "IPC - Porto Alegre", "IPC - Recife", "IPC - Rio de Janeiro", "IPC - Salvador", "IPC - São Paulo")
ipc_regioes_siglas <- c("BR", "MG", "DF", "RS", "PE", "RJ","BA", "SP")
ipc_unidades <- c("Variação","Número Índice")
ipc_unidades_ok <- c("PERCENT", "INDICE")

###INCC

subset_incc<-read.csv2("data/incc.csv")
subset_incc$Estagio<-as.character(subset_incc$Estagio)
subset_incc$Grupo<-as.character(subset_incc$Grupo)
subset_incc$Subgrupo<-as.character(subset_incc$Subgrupo)
subset_incc$Item<-as.character(subset_incc$Item)
subset_incc$tipo<-data.frame(do.call('rbind', strsplit(as.character(subset_incc$Serviço),'-',fixed=TRUE)))[2]
###############boletim_macros#######################################

falencia<-read.csv2("data/boletim_macro_falencia_empresas.csv")
falencia$Porcentagem<-as.numeric(falencia$Porcentagem)

falencia2<-read.csv2("data/boletim_macro_falencia_empresas_2.csv")

monitor_pib<-read.csv2("data/monitor_pib.csv")



# ipc_grupos <- substr(subset_ipc$item_fgv,1,1)
# ipc_subgrupos <- substr(subset_ipc$item_fgv,1,2)
# ipc_itens <- substr(subset_ipc$item_fgv,1,4)
# ipc_subitens <- substr(subset_ipc$item_fgv,1,6)


# names(table(substr(names(subset(table(subset_ipc$Serviço), table(subset_ipc$Serviço) != 0)), 4,5)))
# names(table(subset_ipc$Serviço)), 4,5)

############sONDAGENS#############
sondagens<-read.csv2("data/teste_sondagens.csv")
sondagens[,2]<-as.numeric(as.character(sondagens[,2]))
sondagens[,3]<-as.numeric(as.character(sondagens[,3]))
sondagens[,4]<-as.numeric(as.character(sondagens[,4]))
sondagens[,5]<-as.numeric(as.character(sondagens[,5]))
sondagens[,6]<-as.numeric(as.character(sondagens[,6]))

names(sondagens)<-c("data,Serviço","Indústria","Comércio","Construção","Consumidor")
names_s<-c("Serviço","Indústria","Comércio","Construção","Consumidor")

tsondagem <- ts(sondagens[,-1], start =  c(2013, 09), freq = 12)

######### Favoritos######################

y<-file.exists("data/save_favoritos.csv",stringsAsFactors=F)

if(y==FALSE || dim(read.csv2("data/save_favoritos.csv"))[1]==0){save_favoritos=NULL
}else{save_favoritos=read.csv2("data/save_favoritos.csv",stringsAsFactors=F)}

######## banco central ##################
#setwd("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\smart_IBRE\\Banco_central")
base_bacen<-readRDS("./Banco_central/series_tabela_v2.rds")
#base_bacen<-subset(base_bacen,base_bacen$Per.=="M")
base_bacen<-subset(base_bacen,grepl("BCB",base_bacen$Fonte)==TRUE)
base_bacen[,2]<-as.character(base_bacen[,2])
base_bacen[,1]<-as.numeric(as.character(base_bacen[,1]))
names(base_bacen)[5]<-"Inicio"
names(base_bacen)[6]<-"Fim"
names(base_bacen)[2]<-"Nome"
names(base_bacen)[4]<-"Periodicidade"
source("./Banco_central/banco_central_XML_v2.R")
#setwd("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\smart_IBRE")

############## modelo paramétrico#############
# INCC-EP-DI, IPA-OG-DI e IPC/BR-DI ------------------------------------------- #
# fgv_aux = data.frame(read.csv2("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\06 Modelo Paramétrico\\V2\\data\\base_modelo_percentil_final_testeshiny.csv"))
fgv_aux = data.frame(readRDS("./data\\base_modelo_percentil_final_testeshiny.RDS"))
fgv_dados = ts(data.frame(t(fgv_aux[,10:ncol(fgv_aux)])),
               start = c(2002,01), freq = 12)
colnames(fgv_dados) = fgv_aux$Descricao_Completa

# Auxiliar
teste = NA

# Descrição detalhada das séries (INCC-EP-DI, IPA-OG-DI e IPC/BR-DI)
# fgvinformacao_grupos = read.csv2("V:\\SUEP\\Núcleo de Métodos Estatísticos e Computacionais\\19 Shiny\\06 Modelo Paramétrico\\V2\\data\\FGV_informacao_maiscompleto.csv",stringsAsFactors = FALSE)
fgvinformacao_grupos = readRDS("./data\\FGV_informacao_maiscompleto.RDS")


