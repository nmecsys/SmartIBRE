# pacotes necessários
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(BETS)
library(DBI)
library(RMySQL)
library(DT)
library(dygraphs)


#conexão >> favoritos
conn < - dbConnect(MySQL(), host = "200.20.164.178", db = "smartibredb", user = "smartibre_user", password = "123456", port = 3306)
#adicionar ao favoritos 



user = "jonatha.costa"
add_fav <- function(code,user){
#verificando se já existe o campo pro usuario
  sql <- paste0("select * from favoritos where user_name like '",user,"'")
  aux <- dbGetQuery(conn,sql)
  if(nrow(aux)==0){
    #esse usuario ainda nao criou favoritos
    #vamos criar :)
    sql<-paste0("insert into favoritos() values('",code,"')")
    dbSendQuery(conn,sql)
  }else{
    #ja tem, bora dar um update
    sql <-paste0("update")
    dbSendQuery(conn,sql)
  }
  
}

#deletar nos favoritos

delete_fav <-function(code){
  sql = paste0("")
}
