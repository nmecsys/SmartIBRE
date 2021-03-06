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
library(RColorBrewer)
library(htmlwidgets)
library(zoo)
library(shinyjs)
library(xts)
library(glmulti)
library(xml2)
library(XML)
library(rvest)
library(RCurl)
library(rJava)
#chat uol
#oi - bianquinha
#Olá - Daiane 


##### Constantes

hoje = Sys.Date()
ontem = Sys.Date()-1
####



# gráficos página inicial - preços que vc nem imagina! -------------------------------------------------
dados_ipc <- readRDS("data/dados_ipc.rds")

nomes <- c("CAFEZINHO","ACADEMIA DE GINÁSTICA","GÁS DE BUJÃO","REMÉDIO PARA DIABETE","PRESERVATIVO E LUBRIFICANTE","REVELAÇÃO DE FOTO")
codigos <- c("120703","531103","220103","420133","420509","530505")
produtos <- data.frame(nomes = nomes, codigos = codigos, stringsAsFactors = F)
produtos$codigo0 <- paste0("cod_",produtos$codigos)
variacao <- dados_ipc$variacao$subitens[,paste0("cod_",produtos$codigos)]

data_produto_home <- c(tail(as.Date(variacao),18)[1], tail(as.Date(variacao),1))


# modelo paramétrico
# source("funcoes_parametrico.R")

#conexão >> favoritos
#conn < - dbConnect()
#adicionar ao favoritos

#user = $_GET[$hdsau]

# user = "jonatha.costa"
# add_fav <- function(code,user){
# #verificando se já existe o campo pro usuario
#   sql <- paste0("select * from favoritos where user_name like '",user,"'")
#   aux <- dbGetQuery(conn,sql)
#   if(nrow(aux)==0){
#     #esse usuario ainda nao criou favoritos
#     #vamos criar 
#     sql<-paste0("insert into favoritos() values('",code,"')")
#     dbSendQuery(conn,sql)
#   }else{
#     #ja tem, bora dar um update
#     sql <-paste0("update talbe....")
#     dbSendQuery(conn,sql)
#   }
# 
# }

# deletar nos favoritos

# #delete_fav <-function(code){
#  # sql = paste0("de")
# #  dbSendQuery(conn,sql)
#   return(paste("Séries",series,"foram deletadas dos seus favoritos!"))
# }

regressao_parametrica = function(serie_original, defx, defy, covar = NULL, auto=TRUE){
  
  if(is.character(covar)){
    covar_novo = c()
    for(t in 1:length(covar)){
      for(p in 1:(ncol(serie_original)-2)){
        if(covar[t] == colnames(serie_original)[p+2]){
          covar_novo[t]=p
        }
        if(covar[t] == colnames(serie_original)[1] | covar[t] == colnames(serie_original)[2]){
          stop(paste(covar[t], "não é uma variável válida."))
        }
      }
    }
    if(NA%in%covar_novo | length(covar)!=length(covar_novo)){
      stop("Os nomes das variáveis estão incorretos.")
    }
  }else{covar_novo = covar}
  
  # Periodo das series
  mes = as.numeric(substr(x = serie_original[1,1], start = 6, stop = 7))
  ano = as.numeric(substr(x = serie_original[1,1], start = 1, stop = 4))
  mes_acum = as.numeric(substr(x = serie_original[1,1], start = 6, stop = 7)) + 1
  
  # Numero de covariaveis
  n_covar = ncol(serie_original) - 2
  
  # Variacao do preco do transformador
  var_Transf = NA
  for(k in 1:(length(serie_original[,2])-1)){
    var_Transf[k] = ((serie_original[k+1,2] - serie_original[k,2])/serie_original[k,2])*100
  }
  
  aux_serie = cbind(var_Transf, serie_original[2:nrow(serie_original),3:ncol(serie_original)])
  serie =  ts(aux_serie, start = c(ano, mes), frequency=12)
  
  # S?rie acumulada
  serie_aux = apply(serie, 2, cumsum)
  serie_acum = ts(serie_aux, start = c(ano, mes_acum), frequency=12)
  colnames(serie_acum) = names(serie_original)[2:(n_covar+2)]
  colnames(serie_aux) = names(serie_original)[2:(n_covar+2)]
  
  for(k in 1:ncol(serie_acum)){
    assign(colnames(serie_acum)[k], serie_acum[,k])
  }
  
  if(auto==TRUE){
    if(is.null(covar_novo)){
      
      # Regressao com defy defasagens para y e defx defasagens para x
      def = matrix(c(defx, defy), 1, 2)
      colnames(def) = c("def_x", "def_y")
      def.max = max(defy, defx)
      mes_def = mes_acum + def.max 
      
      if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
      
      if(defy == 0 & defx == 0){
        formula_reg = paste(colnames(serie_acum)[1], "~", colnames(serie_acum)[2])
        for(k in 3:(n_covar+1)){
          formula_reg = paste(formula_reg, "+", colnames(serie_acum)[k])
        }
        aux_reg = formula(formula_reg)
        ajuste_glmulti = glmulti(aux_reg, intercept=TRUE, level=1, method="h", plotty=FALSE,
                                 report=FALSE, crit="BIC")
        #ajuste_glmultifinal = ajuste_glmulti@formulas[[1]]
        #teste = lm(ajuste_glmultifinal)
      }
      
      if(defy > 0 | defx > 0){
        serie_defy = matrix(NA, nrow(serie_acum), defy)
        serie_defx = matrix(NA, nrow(serie_acum), n_covar*defx)
        
        if(defy > 0){
          colunas_defy = NA
          for(j in 1:defy){
            serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
            colunas_defy[j] = paste("Y_def",j, sep="")
          }
          colnames(serie_defy) = colunas_defy
        }
        
        if(defx > 0){
          colunas_defx = c()
          for(i in 1:defx){
            aux = ((i-1)*n_covar + 1):(i*n_covar)
            serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n_covar+1)]
            aux_colunasdefx = c()
            for(l in 1:n_covar){
              aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
            }
            colunas_defx = c(colunas_defx, aux_colunasdefx)
          }
          colnames(serie_defx) = colunas_defx
        }
        
        serie_def= cbind(serie_defy, serie_defx)
        colunas_def = colnames(serie_def)
        
        serie_def = ts(matrix(serie_def[(def.max+1):nrow(serie_acum),],
                              (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx))),
                       start=c(ano, mes_def), frequency=12)
        colnames(serie_def)=colunas_def
        serie_Acum = ts(serie_acum[(def.max+1):nrow(serie_acum),], start=c(ano, mes_def), frequency=12)
        
        for(k in 1:ncol(serie_acum)){
          assign(colnames(serie_acum)[k], serie_Acum[,k])
        }
        for(j in 1:ncol(serie_def)){
          assign(colnames(serie_def)[j], serie_def[,j])
        }
        
        formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
        for(k in 2:n_covar){
          formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
        }
        formula_reg = paste(formula_reg, colnames(serie_acum)[n_covar+1])
        for(l in 1:ncol(serie_def)){
          formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
        }
        
        formula_reg=as.formula(formula_reg)
        ajuste_glmulti = glmulti(formula_reg, intercept=TRUE, level=1, method="h", crit="BIC", 
                                 plotty=FALSE, report=FALSE, fitfunction = "lm", 
                                 maxsize=5)
      }
      
      if(defy == 0 & defx == 0){
        serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
        serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_acum), frequency=12)
        series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
                    serie_ajustada = serie_reg, 
                    ncovar=n_covar, serie_variacao=serie_aux, serie=serie, 
                    serie_acumulada=serie_acum, 
                    preco_acumulado = serie_y, 
                    defasagens = "sem defasagens",
                    resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
                    residuos = ajuste_glmulti@objects[[1]]$residuals)
        #, p1, p2, p3)
        return(series)
        
      } else{
        serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
        serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_def), frequency=12)
        series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
                    serie_ajustada = serie_reg,
                    ncovar=n_covar, serie_variacao=serie_aux, serie=serie, 
                    serie_acumulada=serie_Acum, preco_acumulado = serie_y,
                    serie_defasada = serie_def, defasagens = def,
                    resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
                    residuos = ajuste_glmulti@objects[[1]]$residuals)
        return(series)
      }
    }
    
    if(!is.null(covar_novo)){
      
      # Regressao com defy defasagens para y e defx defasagens para x
      covar_novo = sort(covar_novo)
      def = matrix(c(defx, defy), 1, 2)
      colnames(def) = c("def_x", "def_y")
      def.max = max(defy, defx)
      mes_def = mes_acum + def.max 
      
      if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
      
      if(defy == 0 & defx == 0){
        covar_novo = c(1, (covar_novo+1))
        n = length(covar_novo)
        formula_reg = paste(colnames(serie_acum)[1], "~",colnames(serie_acum)[covar_novo[2]])
        if(n_covar > 1){
          for(k in 3:n){
            formula_reg = paste(formula_reg, "+", colnames(serie_acum)[covar_novo[k]])
          }
        }
        aux_reg = formula(formula_reg)
        ajuste_glmulti = glmulti(aux_reg, intercept=TRUE, level=1, method="h", plotty=FALSE,
                                 report=FALSE, crit="BIC")
      }
      
      if(defy > 0 | defx > 0){
        covar_novo = c(1, (covar_novo+1))
        n = length(covar_novo) - 1 
        serie_acum = serie_acum[,covar_novo]
        
        serie_defy = matrix(NA, nrow(serie_acum), defy)
        serie_defx = matrix(NA, nrow(serie_acum), n*defx)
        
        if(defy > 0){
          colunas_defy = NA
          for(j in 1:defy){
            serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
            colunas_defy[j] = paste("Y_def",j, sep="")
          }
          colnames(serie_defy) = colunas_defys
        }
        
        if(defx > 0){
          colunas_defx = c()
          for(i in 1:defx){
            aux = ((i-1)*n + 1):(i*n)
            serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n+1)]
            aux_colunasdefx = c()
            for(l in 1:n){
              aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
            }
            colunas_defx = c(colunas_defx, aux_colunasdefx)
          }
          colnames(serie_defx) = colunas_defx
        }
        
        serie_def= cbind(serie_defy, serie_defx)
        colunas_def = colnames(serie_def)
        
        serie_def = ts(matrix(serie_def[(def.max+1):nrow(serie_acum),],
                              (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx))),
                       start=c(ano, mes_def), frequency=12)
        colnames(serie_def)=colunas_def
        serie_Acum = ts(serie_acum[(def.max+1):nrow(serie_acum),], 
                        start=c(ano, mes_def), frequency=12)
        
        for(k in 1:ncol(serie_acum)){
          assign(colnames(serie_acum)[k], serie_Acum[,k])
        }
        for(j in 1:ncol(serie_def)){
          assign(colnames(serie_def)[j], serie_def[,j])
        }
        
        formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
        for(k in 2:n){
          formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
        }
        formula_reg = formula_reg = paste(formula_reg, colnames(serie_acum)[n+1])
        for(l in 1:ncol(serie_def)){
          formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
        }
        
        aux_reg=formula(formula_reg)
        ajuste_glmulti = glmulti(aux_reg, intercept=TRUE, level=1, method="h", crit="BIC", 
                                 plotty=FALSE, report=FALSE, fitfunction = "lm", 
                                 maxsize=5)
      }
      
      if(defy == 0 & defx == 0){
        serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
        serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_acum), frequency=12)
        series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
                    serie_ajustada = serie_reg,
                    ncovar=n_covar, serie_variacao=serie_aux, serie=serie,
                    serie_acumulada = serie_acum,
                    preco_acumulado=serie_y, defasagens = "sem defasagens",
                    resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
                    residuos = ajuste_glmulti@objects[[1]]$residuals)
        return(series)
      } else{
        serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
        serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_def), frequency=12)
        series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
                    serie_ajustada = serie_reg,
                    ncovar=n_covar, serie_variacao=serie_aux, serie=serie, 
                    serie_acumulada=serie_Acum, preco_acumulado = serie_y,
                    serie_defasada = serie_def, defasagens = def,
                    resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
                    residuos = ajuste_glmulti@objects[[1]]$residuals)
        return(series)
      }
    }  
  }
  
  if(auto==FALSE){
    if(is.null(covar_novo)){
      
      # Regressao com defy defasagens para y e defx defasagens para x
      def = matrix(c(defx, defy), 1, 2)
      colnames(def) = c("def_x", "def_y")
      def.max = max(defy, defx)
      mes_def = mes_acum + def.max 
      
      if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
      
      if(defy == 0 & defx == 0){
        formula_reg = paste(colnames(serie_acum)[1], "~", colnames(serie_acum)[2])
        for(k in 3:(n_covar+1)){
          formula_reg = paste(formula_reg, "+", colnames(serie_acum)[k])
        }
        aux_reg = formula(formula_reg)
        ajuste_glmulti = lm(aux_reg)
      }
      
      if(defy > 0 | defx > 0){
        serie_defy = matrix(NA, nrow(serie_acum), defy)
        serie_defx = matrix(NA, nrow(serie_acum), n_covar*defx)
        
        if(defy > 0){
          colunas_defy = NA
          for(j in 1:defy){
            serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
            colunas_defy[j] = paste("Y_def",j, sep="")
          }
          colnames(serie_defy) = colunas_defy
        }
        
        if(defx > 0){
          colunas_defx = c()
          for(i in 1:defx){
            aux = ((i-1)*n_covar + 1):(i*n_covar)
            serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n_covar+1)]
            aux_colunasdefx = c()
            for(l in 1:n_covar){
              aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
            }
            colunas_defx = c(colunas_defx, aux_colunasdefx)
          }
          colnames(serie_defx) = colunas_defx
        }
        
        serie_def= cbind(serie_defy, serie_defx)
        colunas_def = colnames(serie_def)
        
        serie_def = ts(matrix(serie_def[(def.max+1):nrow(serie_acum),],
                              (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx))),
                       start=c(ano, mes_def), frequency=12) 
        colnames(serie_def)=colunas_def
        serie_Acum = ts(serie_acum[(def.max+1):nrow(serie_acum),], 
                        start=c(ano, mes_def), frequency=12)
        
        for(k in 1:ncol(serie_acum)){
          assign(colnames(serie_acum)[k], serie_Acum[,k])
        }
        for(j in 1:ncol(serie_def)){
          assign(colnames(serie_def)[j], serie_def[,j])
        }
        
        formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
        for(k in 2:n_covar){
          formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
        }
        formula_reg = paste(formula_reg, colnames(serie_acum)[n_covar+1])
        for(l in 1:ncol(serie_def)){
          formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
        }
        
        aux_reg=formula(formula_reg)
        ajuste_glmulti = lm(aux_reg)
      }
      
      if(defy == 0 & defx == 0){
        serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
        serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_acum), frequency=12)
        series=list(defasagens = "sem defasagens",
                    modelo = formula_reg,
                    ncovar=n_covar, 
                    preco_acumulado = serie_y,
                    residuos = ajuste_glmulti$residuals,
                    resumo_modelo = summary(ajuste_glmulti),
                    serie=serie,
                    serie_acumulada=serie_acum,
                    serie_ajustada = serie_reg,
                    serie_variacao=serie_aux)
        return(series)
        
      } else{
        serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
        serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_def), frequency=12)
        series=list(defasagens = def,
                    modelo = formula_reg,
                    ncovar=n_covar,
                    preco_acumulado = serie_y,
                    residuos = ajuste_glmulti$residuals,
                    resumo_modelo = summary(ajuste_glmulti),
                    serie=serie, 
                    serie_acumulada=serie_Acum,
                    serie_ajustada = serie_reg,
                    serie_defasada = serie_def, 
                    serie_variacao=serie_aux)
        return(series)
      }
    }
    
    if(!is.null(covar_novo)){
      
      # Regressao com defy defasagens para y e defx defasagens para x
      covar_novo = sort(covar_novo)
      def = matrix(c(defx, defy), 1, 2)
      colnames(def) = c("def_x", "def_y")
      def.max = max(defy, defx)
      mes_def = mes_acum + def.max 
      
      if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
      
      if(defy == 0 & defx == 0){
        covar_novo = c(1, (covar_novo+1))
        n = length(covar_novo)
        formula_reg = paste(colnames(serie_acum)[1], "~",colnames(serie_acum)[covar_novo[2]])
        if(n_covar > 1){
          for(k in 3:n){
            formula_reg = paste(formula_reg, "+", colnames(serie_acum)[covar_novo[k]])
          }
        }
        aux_reg = formula(formula_reg)
        ajuste_glmulti = lm(aux_reg)
      }
      
      if(defy > 0 | defx > 0){
        covar_novo = c(1, (covar_novo+1))
        n = length(covar_novo) - 1 
        serie_acum = serie_acum[,covar_novo]
        
        serie_defy = matrix(NA, nrow(serie_acum), defy)
        serie_defx = matrix(NA, nrow(serie_acum), n*defx)
        
        if(defy > 0){
          colunas_defy = NA
          for(j in 1:defy){
            serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
            colunas_defy[j] = paste("Y_def",j, sep="")
          }
          colnames(serie_defy) = colunas_defy
        }
        
        if(defx > 0){
          colunas_defx = c()
          for(i in 1:defx){
            aux = ((i-1)*n + 1):(i*n)
            serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n+1)]
            aux_colunasdefx = c()
            for(l in 1:n){
              aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
            }
            colunas_defx = c(colunas_defx, aux_colunasdefx)
          }
          colnames(serie_defx) = colunas_defx
        }
        
        serie_def= cbind(serie_defy, serie_defx)
        colunas_def = colnames(serie_def)
        serie_def = matrix(serie_def[(def.max+1):nrow(serie_acum),],
                           (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx)))
        colnames(serie_def)=colunas_def
        serie_Acum = serie_acum[(def.max+1):nrow(serie_acum),]
        
        for(k in 1:ncol(serie_acum)){
          assign(colnames(serie_acum)[k], serie_Acum[,k])
        }
        for(j in 1:ncol(serie_def)){
          assign(colnames(serie_def)[j], serie_def[,j])
        }
        
        formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
        for(k in 2:n){
          formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
        }
        formula_reg = formula_reg = paste(formula_reg, colnames(serie_acum)[n+1])
        for(l in 1:ncol(serie_def)){
          formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
        }
        
        aux_reg=formula(formula_reg)
        ajuste_glmulti = lm(aux_reg)
      }
      
      if(defy == 0 & defx == 0){
        serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
        serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_acum), frequency=12)
        series=list(defasagens = "sem defasagens",
                    modelo = formula_reg, 
                    ncovar=n-1, 
                    preco_acumulado = serie_y, 
                    residuos = ajuste_glmulti$residuals,
                    resumo_modelo = summary(ajuste_glmulti),
                    serie=serie, 
                    serie_acumulada=serie_acum,
                    serie_ajustada = serie_reg,
                    serie_variacao=serie_aux)
        return(series)
        
      } else{
        mes_def = mes_acum + def.max 
        serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
        serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_def), frequency=12)
        series=list(defasagens = def,
                    modelo = formula_reg, 
                    ncovar=n_covar, 
                    preco_acumulado = serie_y,
                    residuos = ajuste_glmulti$residuals,
                    resumo_modelo = summary(ajuste_glmulti),
                    serie=serie, 
                    serie_ajustada = serie_reg,
                    serie_acumulada=serie_Acum,
                    serie_defasada = serie_def, 
                    serie_variacao=serie_aux)
        return(series)
      }
    }
  }
  
}
indparam_fixo = function(serie_original, coef, covar=covar_aux){
  
  covar_aux = seq(1, (ncol(serie_original)-2), by=1)
  
  if(length(coef)!=length(covar)){
    stop("O vetor de coeficientes tem dimensão diferente do vetor de covariáveis!")
  }
  
  if(sum(coef) != 1){stop("A soma dos coeficientes deve ser igual a 1!")}
  
  if(is.character(covar)){
    covar_novo = c()
    for(t in 1:length(covar)){
      for(p in 1:(ncol(serie_original)-2)){
        if(covar[t] == colnames(serie_original)[p+2]){
          covar_novo[t]=p
        }
        if(covar[t] == colnames(serie_original)[1] | covar[t] == colnames(serie_original)[2]){
          stop(paste(covar[t], "não é uma variável válida."))
        }
      }
    }
    if(NA%in%covar_novo | length(covar)!=length(covar_novo)){
      stop("Os nomes das variáveis estão incorretos.")
    }
  }else{covar_novo = covar}
  
  covar_novo = sort(covar_novo)
  
  # Periodo da serie
  mes_acum = as.numeric(substr(x = serie_original$Periodo[1], start = 6, stop = 7)) + 1
  ano = as.numeric(substr(x = serie_original$Periodo[1], start = 1, stop = 4))
  
  # Calcular a variacao do preco do transformador
  var_Transf = NA
  for(k in 1:(length(serie_original[,2])-1)){
    var_Transf[k] = ((serie_original[k+1,2] - serie_original[k,2])/serie_original[k,2])*100
  }
  
  # Especificar inicio da series acumulada
  mes_acum = as.numeric(substr(x = serie_original$Periodo[1], start = 6, stop = 7)) + 1
  
  preco_aux = matrix(NA, nrow(serie_original), length(coef))
  colnames(preco_aux) = names(serie_original)[3:(length(coef)+2)]
  for(i in 1:length(coef)){
    preco_aux[,i] = coef[i]*serie_original[,(covar_novo[i]+2)]
  }
  Preco_Parametrico = apply(preco_aux, 1, sum)
  PrecoAcum_Parametrico = cumsum(Preco_Parametrico)
  PrecoAcum_Parametrico = ts(PrecoAcum_Parametrico[-1], start = c(ano, mes_acum), frequency=12)
  
  Preco_Acum =  ts(cumsum(var_Transf), start = c(ano, mes_acum), frequency=12)
  
  coef_aux = matrix(coef, 1, length(coef))
  colnames(coef_aux) = colnames(serie_original)[(covar_novo+2)]
  results = list(Preco_Parametrico = Preco_Parametrico, 
                 PrecoParametrico_acumulado = PrecoAcum_Parametrico, 
                 Preco_Transformador = Preco_Acum,
                 Coeficientes = coef_aux)
  return(results)
}



cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


crawler_blog_ibre <- function(){
  url_base <- "http://blogdoibre.fgv.br/rss/posts/posts-rss.xml"
  # html<-read_html(x = url_base)
  # feeds_link <- html %>% html_nodes(css = "item")
  # aux <- stringr::str_extract(string =feeds_link,pattern = "<link>.*")
  # links <- stringr::str_replace(string = aux,pattern = "<link>",replacement ="")
  
  
  xml  <- read_xml(x = url_base,encoding = "UTF-8")
  link <- xml %>% html_nodes("item link") %>% html_text(trim = T)
  manchete <- xml %>% html_nodes("item title") %>% html_text(trim=T)
  descricao <- xml %>% html_nodes("item description") %>% html_text(trim=T) %>% cleanFun()
  date <- xml %>% html_nodes("item pubDate") %>% html_text(trim=T) %>% cleanFun()
  
  manchete= iconv(manchete,from="UTF-8",to="latin1")
  descricao = iconv(descricao,from="UTF-8",to="latin1")
  novo_df <- data.frame(link = link,
                        manchete = manchete,
                        descricao = descricao,
                        date = date,
                        stringsAsFactors = FALSE
                        )
  #conn = connection()
  #message("Adicionando novas noticias no banco")
  #
  #DBI::dbWriteTable(conn,name = "noticiasBI",novo_df,overwrite = TRUE)
  
  #invisible(dbDisconnect(conn))
  return(novo_df)
}

noticias = crawler_blog_ibre()
noticias_bi = noticias[1:3,]

noticias_bi$descricao[1] <- paste0(strsplit(as.character(noticias_bi$descricao[1]), " ")[[1]][1:50], collapse = " ")
noticias_bi$descricao[2] <- paste0(strsplit(as.character(noticias_bi$descricao[2]), " ")[[1]][1:50], collapse = " ")
noticias_bi$descricao[3] <- paste0(strsplit(as.character(noticias_bi$descricao[3]), " ")[[1]][1:50], collapse = " ")


# #Noticias do blog do ibre, pegando as 3 mais atuais
# connection = function(){
#   conn = dbConnect(MySQL(),db="smartibredb",user="smartibre_user",password="123456",host="200.20.164.178",port=3306)
# }
#   conn = connection()
#   noticias_bi = DBI::dbGetQuery(conn,"Select link,manchete,descricao,date from noticiasBI order by date desc limit 10")
 #noticias_bi$manchete = iconv(noticias_bi$manchete,from="UTF-8",to="latin1")
 #noticias_bi$descricao = iconv(noticias_bi$descricao,from="UTF-8",to="latin1")
#  noticias_bi = unique(noticias_bi)
#  invisible(dbDisconnect(conn))


 
