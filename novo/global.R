# pacotes necessários
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(BETS)
library(DBI)
library(RMySQL)

# conexão >> favoritos
conn <- dbConnect(MySQL(), host = "200.20.164.178", db = "smartibredb", user = "smartibre_user", password = "123456", port = 3306)