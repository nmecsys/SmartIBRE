# pacotes necessários
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(BETS)
library(DBI)
library(RMySQL)
library(DT)

# conexão >> favoritos
# conn <- dbConnect(MySQL(), host = "200.20.164.178", db = "smartibredb", user = "smartibre_user", password = "123456", port = 3306)


#dbSendQuery(conn,"insert into favoritos(series) values('c(1,2,3)')")
#supondo que conseguimos identificar o usuario, atraves do seu username
# dbGetQuery(conn,"select * from favoritos where username = 'username'")
# 
# dbListFields(conn,'favoritos')
# dbGetQuery(conn,'select * from favoritos')
