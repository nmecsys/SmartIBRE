

# PARTE DA PREVISÃO AUTO-ARIMA ----------------------------------------------------------------

auto_arima <- function(serie){
  auto <- auto.arima(serie)
  return(auto$arma)
}

# Função do teste t para o modelo ARIMA
t_test <- function(serie, modelo_arima, n_x = 0){
  
  # Esta funçãoo faz o teste de Significância dos parâmetros de um modelo ARIMA
  # n_x = o número de variáveis exógenas
  
  # Estatística T
  coef <- modelo_arima$coef
  se <- sqrt(diag(modelo_arima$var.coef))
  t <- abs(coef/se)
  # Teste t
  ok <- t > qt(0.975, length(serie) - sum(modelo_arima$arma[c(1,2,3,4,6,7)]) - n_x)
  ok[which(ok == TRUE)] <- "Sim"
  ok[which(ok == FALSE)] <- "Não"
  resul <- data.frame(Coefiente = coef, Desvio = se, t = t, Significativo = ok )
  return(resul)
  
}
# FIM AUTO-ARIMA #


# MODELO PARAMÉTRICO --------------------------------------------------------------------------
# S?rie temporal
# regressao_parametrica = function(serie_original, defx, defy, covar=covar_aux, auto=auto_aux){
#   
#   covar_aux = c()
#   auto_aux = TRUE
#   
#   if(is.character(covar)){
#     covar_novo = c()
#     for(t in 1:length(covar)){
#       for(p in 1:(ncol(serie_original)-2)){
#         if(covar[t] == colnames(serie_original)[p+2]){
#           covar_novo[t]=p
#         }
#         if(covar[t] == colnames(serie_original)[1] | covar[t] == colnames(serie_original)[2]){
#           stop(paste(covar[t], "não é uma variável válida."))
#         }
#       }
#     }
#     if(NA%in%covar_novo | length(covar)!=length(covar_novo)){
#       stop("Os nomes das variáveis estão incorretos.")
#     }
#   }else{covar_novo = covar}
#   
#   # Per?odo das s?ries
#   mes = as.numeric(substr(x = serie_original[1,1], start = 6, stop = 7))
#   ano = as.numeric(substr(x = serie_original[1,1], start = 1, stop = 4))
#   mes_acum = as.numeric(substr(x = serie_original[1,1], start = 6, stop = 7)) + 1
#   
#   # N?mero de covari?veis
#   n_covar = ncol(serie_original) - 2
#   
#   # Varia??o do pre?o do transformador
#   var_Transf = NA
#   for(k in 1:(length(serie_original[,2])-1)){
#     var_Transf[k] = ((serie_original[k+1,2] - serie_original[k,2])/serie_original[k,2])*100
#   }
#   
#   aux_serie = cbind(var_Transf, serie_original[2:nrow(serie_original),3:ncol(serie_original)])
#   serie =  ts(aux_serie, start = c(ano, mes), frequency=12)
#   
#   # S?rie acumulada
#   serie_aux = apply(serie, 2, cumsum)
#   serie_acum = ts(serie_aux, start = c(ano, mes_acum), frequency=12)
#   colnames(serie_acum) = names(serie_original)[2:(n_covar+2)]
#   colnames(serie_aux) = names(serie_original)[2:(n_covar+2)]
#   
#   for(k in 1:ncol(serie_acum)){
#     assign(colnames(serie_acum)[k], serie_acum[,k])
#   }
#   
#   if(auto==TRUE){
#     if(is.null(covar_novo)){
#       
#       # Regress?o com defy defasagens para y e defx defasagens para x
#       def = matrix(c(defx, defy), 1, 2)
#       colnames(def) = c("def_x", "def_y")
#       def.max = max(defy, defx)
#       mes_def = mes_acum + def.max 
#       
#       if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
#       
#       if(defy == 0 & defx == 0){
#         formula_reg = paste(colnames(serie_acum)[1], "~", colnames(serie_acum)[2])
#         for(k in 3:(n_covar+1)){
#           formula_reg = paste(formula_reg, "+", colnames(serie_acum)[k])
#         }
#         aux_reg = formula(formula_reg)
#         ajuste_glmulti = glmulti(aux_reg, intercept=TRUE, level=1, method="h", plotty=FALSE,
#                                  report=FALSE, crit="BIC")
#         #ajuste_glmultifinal = ajuste_glmulti@formulas[[1]]
#         #teste = lm(ajuste_glmultifinal)
#       }
#       
#       if(defy > 0 | defx > 0){
#         serie_defy = matrix(NA, nrow(serie_acum), defy)
#         serie_defx = matrix(NA, nrow(serie_acum), n_covar*defx)
#         
#         if(defy > 0){
#           colunas_defy = NA
#           for(j in 1:defy){
#             serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
#             colunas_defy[j] = paste("Y_def",j, sep="")
#           }
#           colnames(serie_defy) = colunas_defy
#         }
#         
#         if(defx > 0){
#           colunas_defx = c()
#           for(i in 1:defx){
#             aux = ((i-1)*n_covar + 1):(i*n_covar)
#             serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n_covar+1)]
#             aux_colunasdefx = c()
#             for(l in 1:n_covar){
#               aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
#             }
#             colunas_defx = c(colunas_defx, aux_colunasdefx)
#           }
#           colnames(serie_defx) = colunas_defx
#         }
#         
#         serie_def= cbind(serie_defy, serie_defx)
#         colunas_def = colnames(serie_def)
#         
#         serie_def = ts(matrix(serie_def[(def.max+1):nrow(serie_acum),],
#                               (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx))),
#                        start=c(ano, mes_def), frequency=12)
#         colnames(serie_def)=colunas_def
#         serie_Acum = ts(serie_acum[(def.max+1):nrow(serie_acum),], start=c(ano, mes_def), frequency=12)
#         
#         for(k in 1:ncol(serie_acum)){
#           assign(colnames(serie_acum)[k], serie_Acum[,k])
#         }
#         for(j in 1:ncol(serie_def)){
#           assign(colnames(serie_def)[j], serie_def[,j])
#         }
#         
#         formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
#         for(k in 2:n_covar){
#           formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
#         }
#         formula_reg = paste(formula_reg, colnames(serie_acum)[n_covar+1])
#         for(l in 1:ncol(serie_def)){
#           formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
#         }
#         
#         formula_reg=as.formula(formula_reg)
#         ajuste_glmulti = glmulti(formula_reg, intercept=TRUE, level=1, method="h", crit="BIC", 
#                                  plotty=FALSE, report=FALSE, fitfunction = "lm", 
#                                  maxsize=5)
#       }
#       
#       if(defy == 0 & defx == 0){
#         serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
#         serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_acum), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                            as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                            "1 months")),
#         #                         ajuste_glmulti@objects[[1]]$fitted.values, 
#         #                         ajuste_glmulti@objects[[1]]$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida\\Resultados_RegParam.xlsx",
#         #                  sheetName = "Serie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
#                     serie_ajustada = serie_reg, 
#                     ncovar=n_covar, serie_variacao=serie_aux, serie=serie, 
#                     serie_acumulada=serie_acum, 
#                     preco_acumulado = serie_y, 
#                     defasagens = "sem defasagens",
#                     resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
#                     residuos = ajuste_glmulti@objects[[1]]$residuals)
#         #, p1, p2, p3)
#         return(series)
#         
#       } else{
#         serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
#         serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_def), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                                  "1 months")),
#         #                              ajuste_glmulti@objects[[1]]$fitted.values, 
#         #                              ajuste_glmulti@objects[[1]]$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida\\Resultados_RegParam.xlsx",
#         #                  sheetName = "Serie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
#                     serie_ajustada = serie_reg,
#                     ncovar=n_covar, serie_variacao=serie_aux, serie=serie, 
#                     serie_acumulada=serie_Acum, preco_acumulado = serie_y,
#                     serie_defasada = serie_def, defasagens = def,
#                     resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
#                     residuos = ajuste_glmulti@objects[[1]]$residuals)
#         return(series)
#       }
#     }
#     
#     if(!is.null(covar_novo)){
#       
#       # Regress?o com defy defasagens para y e defx defasagens para x
#       covar_novo = sort(covar_novo)
#       def = matrix(c(defx, defy), 1, 2)
#       colnames(def) = c("def_x", "def_y")
#       def.max = max(defy, defx)
#       mes_def = mes_acum + def.max 
#       
#       if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
#       
#       if(defy == 0 & defx == 0){
#         covar_novo = c(1, (covar_novo+1))
#         n = length(covar_novo)
#         formula_reg = paste(colnames(serie_acum)[1], "~",colnames(serie_acum)[covar_novo[2]])
#         if(n_covar > 1){
#           for(k in 3:n){
#             formula_reg = paste(formula_reg, "+", colnames(serie_acum)[covar_novo[k]])
#           }
#         }
#         aux_reg = formula(formula_reg)
#         ajuste_glmulti = glmulti(aux_reg, intercept=TRUE, level=1, method="h", plotty=FALSE,
#                                  report=FALSE, crit="BIC")
#         #ajuste_glmultifinal = ajuste_glmulti@formulas[[1]]
#         #teste = lm(ajuste_glmultifinal)
#       }
#       
#       if(defy > 0 | defx > 0){
#         covar_novo = c(1, (covar_novo+1))
#         n = length(covar_novo) - 1 
#         serie_acum = serie_acum[,covar_novo]
#         
#         serie_defy = matrix(NA, nrow(serie_acum), defy)
#         serie_defx = matrix(NA, nrow(serie_acum), n*defx)
#         
#         if(defy > 0){
#           colunas_defy = NA
#           for(j in 1:defy){
#             serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
#             colunas_defy[j] = paste("Y_def",j, sep="")
#           }
#           colnames(serie_defy) = colunas_defy
#         }
#         
#         if(defx > 0){
#           colunas_defx = c()
#           for(i in 1:defx){
#             aux = ((i-1)*n + 1):(i*n)
#             serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n+1)]
#             aux_colunasdefx = c()
#             for(l in 1:n){
#               aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
#             }
#             colunas_defx = c(colunas_defx, aux_colunasdefx)
#           }
#           colnames(serie_defx) = colunas_defx
#         }
#         
#         serie_def= cbind(serie_defy, serie_defx)
#         colunas_def = colnames(serie_def)
#         
#         serie_def = ts(matrix(serie_def[(def.max+1):nrow(serie_acum),],
#                               (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx))),
#                        start=c(ano, mes_def), frequency=12)
#         colnames(serie_def)=colunas_def
#         serie_Acum = ts(serie_acum[(def.max+1):nrow(serie_acum),], 
#                         start=c(ano, mes_def), frequency=12)
#         
#         for(k in 1:ncol(serie_acum)){
#           assign(colnames(serie_acum)[k], serie_Acum[,k])
#         }
#         for(j in 1:ncol(serie_def)){
#           assign(colnames(serie_def)[j], serie_def[,j])
#         }
#         
#         formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
#         for(k in 2:n){
#           formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
#         }
#         formula_reg = formula_reg = paste(formula_reg, colnames(serie_acum)[n+1])
#         for(l in 1:ncol(serie_def)){
#           formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
#         }
#         
#         aux_reg=formula(formula_reg)
#         ajuste_glmulti = glmulti(aux_reg, intercept=TRUE, level=1, method="h", crit="BIC", 
#                                  plotty=FALSE, report=FALSE, fitfunction = "lm", 
#                                  maxsize=5)
#       }
#       
#       if(defy == 0 & defx == 0){
#         serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
#         serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_acum), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                                  "1 months")),
#         #                              ajuste_glmulti@objects[[1]]$fitted.values, 
#         #                              ajuste_glmulti@objects[[1]]$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida\\Resultados_RegParam.xlsx",
#         #                  sheetName = "Serie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
#                     serie_ajustada = serie_reg,
#                     ncovar=n_covar, serie_variacao=serie_aux, serie=serie,
#                     serie_acumulada = serie_acum,
#                     preco_acumulado=serie_y, defasagens = "sem defasagens",
#                     resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
#                     residuos = ajuste_glmulti@objects[[1]]$residuals)
#         return(series)
#       } else{
#         serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
#         serie_reg = ts(ajuste_glmulti@objects[[1]]$fitted.values, start = c(ano, mes_def), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                                  "1 months")),
#         #                              ajuste_glmulti@objects[[1]]$fitted.values, 
#         #                              ajuste_glmulti@objects[[1]]$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida\\Resultados_RegParam.xlsx",
#         #                  sheetName = "Serie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(modelo = paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]), 
#                     serie_ajustada = serie_reg,
#                     ncovar=n_covar, serie_variacao=serie_aux, serie=serie, 
#                     serie_acumulada=serie_Acum, preco_acumulado = serie_y,
#                     serie_defasada = serie_def, defasagens = def,
#                     resumo_modelo = summary(ajuste_glmulti@objects[[1]]),
#                     residuos = ajuste_glmulti@objects[[1]]$residuals)
#         return(series)
#       }
#     }  
#   }
#   
#   if(auto==FALSE){
#     if(is.null(covar_novo)){
#       
#       # Regress?o com defy defasagens para y e defx defasagens para x
#       def = matrix(c(defx, defy), 1, 2)
#       colnames(def) = c("def_x", "def_y")
#       def.max = max(defy, defx)
#       mes_def = mes_acum + def.max 
#       
#       if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
#       
#       if(defy == 0 & defx == 0){
#         formula_reg = paste(colnames(serie_acum)[1], "~", colnames(serie_acum)[2])
#         for(k in 3:(n_covar+1)){
#           formula_reg = paste(formula_reg, "+", colnames(serie_acum)[k])
#         }
#         aux_reg = formula(formula_reg)
#         ajuste_glmulti = lm(aux_reg)
#       }
#       
#       if(defy > 0 | defx > 0){
#         serie_defy = matrix(NA, nrow(serie_acum), defy)
#         serie_defx = matrix(NA, nrow(serie_acum), n_covar*defx)
#         
#         if(defy > 0){
#           colunas_defy = NA
#           for(j in 1:defy){
#             serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
#             colunas_defy[j] = paste("Y_def",j, sep="")
#           }
#           colnames(serie_defy) = colunas_defy
#         }
#         
#         if(defx > 0){
#           colunas_defx = c()
#           for(i in 1:defx){
#             aux = ((i-1)*n_covar + 1):(i*n_covar)
#             serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n_covar+1)]
#             aux_colunasdefx = c()
#             for(l in 1:n_covar){
#               aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
#             }
#             colunas_defx = c(colunas_defx, aux_colunasdefx)
#           }
#           colnames(serie_defx) = colunas_defx
#         }
#         
#         serie_def= cbind(serie_defy, serie_defx)
#         colunas_def = colnames(serie_def)
#         
#         serie_def = ts(matrix(serie_def[(def.max+1):nrow(serie_acum),],
#                               (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx))),
#                        start=c(ano, mes_def), frequency=12) 
#         colnames(serie_def)=colunas_def
#         serie_Acum = ts(serie_acum[(def.max+1):nrow(serie_acum),], 
#                         start=c(ano, mes_def), frequency=12)
#         
#         for(k in 1:ncol(serie_acum)){
#           assign(colnames(serie_acum)[k], serie_Acum[,k])
#         }
#         for(j in 1:ncol(serie_def)){
#           assign(colnames(serie_def)[j], serie_def[,j])
#         }
#         
#         formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
#         for(k in 2:n_covar){
#           formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
#         }
#         formula_reg = paste(formula_reg, colnames(serie_acum)[n_covar+1])
#         for(l in 1:ncol(serie_def)){
#           formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
#         }
#         
#         aux_reg=formula(formula_reg)
#         ajuste_glmulti = lm(aux_reg)
#       }
#       
#       if(defy == 0 & defx == 0){
#         serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
#         serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_acum), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                                  "1 months")),
#         #                              ajuste_glmulti$fitted.values, 
#         #                              ajuste_glmulti$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida\\Resultados_RegParam.xlsx",
#         #                  sheetName = "Serie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(defasagens = "sem defasagens",
#                     modelo = formula_reg,
#                     ncovar=n_covar, 
#                     preco_acumulado = serie_y,
#                     residuos = ajuste_glmulti$residuals,
#                     resumo_modelo = summary(ajuste_glmulti),
#                     serie=serie,
#                     serie_acumulada=serie_acum,
#                     serie_ajustada = serie_reg,
#                     serie_variacao=serie_aux)
#         return(series)
#         
#       } else{
#         serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
#         serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_def), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                                  "1 months")),
#         #                              ajuste_glmulti$fitted.values, 
#         #                              ajuste_glmulti$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida\\Resultados_RegParam.xlsx",
#         #                  sheetName = "Serie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(defasagens = def,
#                     modelo = formula_reg,
#                     ncovar=n_covar,
#                     preco_acumulado = serie_y,
#                     residuos = ajuste_glmulti$residuals,
#                     resumo_modelo = summary(ajuste_glmulti),
#                     serie=serie, 
#                     serie_acumulada=serie_Acum,
#                     serie_ajustada = serie_reg,
#                     serie_defasada = serie_def, 
#                     serie_variacao=serie_aux)
#         return(series)
#       }
#     }
#     
#     if(!is.null(covar_novo)){
#       
#       # Regress?o com defy defasagens para y e defx defasagens para x
#       covar_novo = sort(covar_novo)
#       def = matrix(c(defx, defy), 1, 2)
#       colnames(def) = c("def_x", "def_y")
#       def.max = max(defy, defx)
#       mes_def = mes_acum + def.max 
#       
#       if(defy < 0 | defx < 0){stop("As defasagens defx e defy não podem ser negativas!")}
#       
#       if(defy == 0 & defx == 0){
#         covar_novo = c(1, (covar_novo+1))
#         n = length(covar_novo)
#         formula_reg = paste(colnames(serie_acum)[1], "~",colnames(serie_acum)[covar_novo[2]])
#         if(n_covar > 1){
#           for(k in 3:n){
#             formula_reg = paste(formula_reg, "+", colnames(serie_acum)[covar_novo[k]])
#           }
#         }
#         aux_reg = formula(formula_reg)
#         ajuste_glmulti = lm(aux_reg)
#         #ajuste_glmultifinal = ajuste_glmulti@formulas[[1]]
#         #teste = lm(ajuste_glmultifinal)
#       }
#       
#       if(defy > 0 | defx > 0){
#         covar_novo = c(1, (covar_novo+1))
#         n = length(covar_novo) - 1 
#         serie_acum = serie_acum[,covar_novo]
#         
#         serie_defy = matrix(NA, nrow(serie_acum), defy)
#         serie_defx = matrix(NA, nrow(serie_acum), n*defx)
#         
#         if(defy > 0){
#           colunas_defy = NA
#           for(j in 1:defy){
#             serie_defy[(j+1):nrow(serie_defy),j] = serie_acum[1:(nrow(serie_defy)-j),1]
#             colunas_defy[j] = paste("Y_def",j, sep="")
#           }
#           colnames(serie_defy) = colunas_defy
#         }
#         
#         if(defx > 0){
#           colunas_defx = c()
#           for(i in 1:defx){
#             aux = ((i-1)*n + 1):(i*n)
#             serie_defx[(i+1):nrow(serie_defx),aux] = serie_acum[1:(nrow(serie_defx)-i),2:(n+1)]
#             aux_colunasdefx = c()
#             for(l in 1:n){
#               aux_colunasdefx = c(aux_colunasdefx, paste(colnames(serie_acum)[l+1], "_def",i, sep=""))
#             }
#             colunas_defx = c(colunas_defx, aux_colunasdefx)
#           }
#           colnames(serie_defx) = colunas_defx
#         }
#         
#         serie_def= cbind(serie_defy, serie_defx)
#         colunas_def = colnames(serie_def)
#         serie_def = matrix(serie_def[(def.max+1):nrow(serie_acum),],
#                            (nrow(serie_acum)-def.max), (ncol(serie_defy) + ncol(serie_defx)))
#         colnames(serie_def)=colunas_def
#         serie_Acum = serie_acum[(def.max+1):nrow(serie_acum),]
#         
#         for(k in 1:ncol(serie_acum)){
#           assign(colnames(serie_acum)[k], serie_Acum[,k])
#         }
#         for(j in 1:ncol(serie_def)){
#           assign(colnames(serie_def)[j], serie_def[,j])
#         }
#         
#         formula_reg = paste(colnames(serie_acum)[1],  "~", sep="")
#         for(k in 2:n){
#           formula_reg = paste(formula_reg, colnames(serie_acum)[k], " + ")
#         }
#         formula_reg = formula_reg = paste(formula_reg, colnames(serie_acum)[n+1])
#         for(l in 1:ncol(serie_def)){
#           formula_reg = paste(formula_reg, " + ", colnames(serie_def)[l])
#         }
#         
#         aux_reg=formula(formula_reg)
#         ajuste_glmulti = lm(aux_reg)
#       }
#       
#       if(defy == 0 & defx == 0){
#         serie_y = ts(serie_acum[,1], start=c(ano, mes_acum), frequency=12)
#         serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_acum), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                                  "1 months")),
#         #                              ajuste_glmulti$fitted.values, 
#         #                              ajuste_glmulti$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida\\Resultados_RegParam.xlsx",
#         #                  sheetName = "Serie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(defasagens = "sem defasagens",
#                     modelo = formula_reg, 
#                     ncovar=n-1, 
#                     preco_acumulado = serie_y, 
#                     residuos = ajuste_glmulti$residuals,
#                     resumo_modelo = summary(ajuste_glmulti),
#                     serie=serie, 
#                     serie_acumulada=serie_acum,
#                     serie_ajustada = serie_reg,
#                     serie_variacao=serie_aux)
#         return(series)
#         
#       } else{
#         mes_def = mes_acum + def.max 
#         serie_y = ts(serie_Acum[,1], start=c(ano, mes_def), frequency=12)
#         serie_reg = ts(ajuste_glmulti$fitted.values, start = c(ano, mes_def), frequency=12)
#         #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
#         #p2 = lines(serie_reg, col="blue", lty="dotted")
#         #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
#         #            col = c("black", "blue"), cex = 0.7)
#         #p1
#         #p2
#         #p3
#         
#         #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
#         #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
#         #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
#         #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
#         #                                                  "1 months")),
#         #                              ajuste_glmulti$fitted.values, 
#         #                              ajuste_glmulti$residuals)
#         #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
#         #xlsx::write.xlsx( series_regressao,
#         #                  file = "Saida/Resultados_RegParam.xlsx",
#         #                  sheetName = "S?rie Ajustada",
#         #                  row.names = FALSE,
#         #                  col.names = TRUE)
#         
#         series=list(defasagens = def,
#                     modelo = formula_reg, 
#                     ncovar=n_covar, 
#                     preco_acumulado = serie_y,
#                     residuos = ajuste_glmulti$residuals,
#                     resumo_modelo = summary(ajuste_glmulti),
#                     serie=serie, 
#                     serie_ajustada = serie_reg,
#                     serie_acumulada=serie_Acum,
#                     serie_defasada = serie_def, 
#                     serie_variacao=serie_aux)
#         return(series)
#       }
#     }
#   }
#   
# }
# 
# 
# # ?ndice fixo
# indparam_fixo = function(serie_original, coef, covar=covar_aux){
#   
#   covar_aux = seq(1, (ncol(serie_original)-2), by=1)
#   
#   if(length(coef)!=length(covar)){
#     stop("O vetor de coeficientes tem dimensão diferente do vetor de covariáveis!")
#   }
#   
#   if(sum(coef) != 1){stop("A soma dos coeficientes deve ser igual a 1!")}
#   
#   if(is.character(covar)){
#     covar_novo = c()
#     for(t in 1:length(covar)){
#       for(p in 1:(ncol(serie_original)-2)){
#         if(covar[t] == colnames(serie_original)[p+2]){
#           covar_novo[t]=p
#         }
#         if(covar[t] == colnames(serie_original)[1] | covar[t] == colnames(serie_original)[2]){
#           stop(paste(covar[t], "não é uma variável válida."))
#         }
#       }
#     }
#     if(NA%in%covar_novo | length(covar)!=length(covar_novo)){
#       stop("Os nomes das variáveis estão incorretos.")
#     }
#   }else{covar_novo = covar}
#   
#   covar_novo = sort(covar_novo)
#   
#   # Per?odo da s?rie
#   mes_acum = as.numeric(substr(x = serie_original$Periodo[1], start = 6, stop = 7)) + 1
#   ano = as.numeric(substr(x = serie_original$Periodo[1], start = 1, stop = 4))
#   
#   # Calcular a varia??o do pre?o do transformador
#   var_Transf = NA
#   for(k in 1:(length(serie_original[,2])-1)){
#     var_Transf[k] = ((serie_original[k+1,2] - serie_original[k,2])/serie_original[k,2])*100
#   }
#   
#   # Especificar ?nicio da s?ries acumulada
#   mes_acum = as.numeric(substr(x = serie_original$Periodo[1], start = 6, stop = 7)) + 1
#   
#   preco_aux = matrix(NA, nrow(serie_original), length(coef))
#   colnames(preco_aux) = names(serie_original)[3:(length(coef)+2)]
#   #preco_aux[,1] = rep(coef[1], nrow(serie_original))
#   for(i in 1:length(coef)){
#     preco_aux[,i] = coef[i]*serie_original[,(covar_novo[i]+2)]
#   }
#   Preco_Parametrico = apply(preco_aux, 1, sum)
#   PrecoAcum_Parametrico = cumsum(Preco_Parametrico)
#   PrecoAcum_Parametrico = ts(PrecoAcum_Parametrico[-1], start = c(ano, mes_acum), frequency=12)
#   
#   Preco_Acum =  ts(cumsum(var_Transf), start = c(ano, mes_acum), frequency=12)
#   
#   coef_aux = matrix(coef, 1, length(coef))
#   colnames(coef_aux) = colnames(serie_original)[(covar_novo+2)]
#   
# #   p1 = plot(Preco_Acum, main=" ", xlab=" ", ylab=" ")
# #   p2 = lines(PrecoAcum_Parametrico, col="red", lty=2)
# #   p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Parametrico"), lty = c(1,2), lwd = 1,
# #               col = c("black", "red"), cex = 0.7)
# #   
# #   ano_final = ano + length(Preco_Acum)%/%12
# #   mes_final = mes_acum + (length(Preco_Acum)%%12) - 1
# #   serie_indparam = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_acum, "01", sep="/")),
# #                                                   as.Date(paste(ano_final, mes_final, "01", sep="/")),
# #                                                   "1 months")),
# #                               Preco_Acum, PrecoAcum_Parametrico)
# #   colnames(serie_indparam) = c("Período", "Preço Acumulado","Índice Paramétrico Acumulado")
#   
# #   xlsx::write.xlsx( serie_indparam,
# #                     file = "Saida\\Resultados_IndFixo.xlsx",
# #                     sheetName = "Series",
# #                     row.names = FALSE,
# #                     col.names = TRUE)
#   
#   results = list(Preco_Parametrico = Preco_Parametrico, 
#                  PrecoParametrico_acumulado = PrecoAcum_Parametrico, 
#                  Preco_Transformador = Preco_Acum,
#                  Coeficientes = coef_aux)
#   return(results)
# }


# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(shiny, dygraphs, xlsx, foreign, zoo, shinythemes, quantmod, shinydashboard, datasets, 
#                glmulti, forecast, DT, RColorBrewer, lubridate, stringr, dplyr, xtable, stargazer, chron)

ajuste_rapido <- function(st, transform.function = "auto", outlier = "", regression.aictest = c("td","easter"),...){
  
  seas(st, transform.function = transform.function, outlier = outlier, regression.aictest = regression.aictest, ...)
  
}


# Modelo Paramétrico (final)
cluster_function = function(series, cov_max = num_max,
                            var_max = num_var, 
                            variacao = var,
                            updateProgress = NULL){
  
  num_max = 15
  num_var = 15
  var = TRUE
  
  assign("cov_max", cov_max, envir = .GlobalEnv)
  
  series_aux = series
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(lubridate, glmulti, dygraphs, stringr, dplyr)
  
  if(variacao == TRUE){
    series = series}else{
      series[2:nrow(series),2] = ((series[2:nrow(series),2]/series[1:(nrow(series) - 1),2]) - 1)*100
      series = series[-1,]
    }
  
  # Transformando os dados em série temporal
  series_ts = ts(series[,-1], start = c(year(series[1,1]), month(series[1,1])), freq = 12)
  
  # Calculando a correlação cruzada com lag máximo igual a 2
  cor_cruzada = matrix(NA, (ncol(series)-1), 3)
  colnames(cor_cruzada) = c("t-2", "t-1", "t")
  rownames(cor_cruzada) = paste("serie", 1:ncol(series_ts))
  for(k in 2:nrow(cor_cruzada)){
    
    aux_ccf = ccf(series_ts[,k], series_ts[,1], lag.max = 2, plot = FALSE)
    cor_cruzada[k,] = c(aux_ccf$acf)[1:3]
    rownames(cor_cruzada)[k] = colnames(series_ts)[k]
    
  }
  cor_cruzada = cor_cruzada[-1,]
  
  corr_series = matrix(c(cor_cruzada[,"t"], cor_cruzada[,"t-1"], cor_cruzada[,"t-2"]))
  rownames(corr_series) = c(rownames(cor_cruzada), 
                            paste0(rownames(cor_cruzada), "_def1"),
                            paste0(rownames(cor_cruzada), "_def2"))
  
  # Defasando as séries (lags 1 e 2 somente)
  series_def1 = stats::lag(series_ts, -1)
  series_def2 = stats::lag(series_ts, -2)
  
  series = ts(na.omit(cbind(series_ts, series_def1, series_def2)),
              start = c(as.integer(tsp(series_def2)[1]),
                        as.integer((tsp(series_def2)[1]%%1)/0.083) + 1), freq = 12)
  colnames(series) = c(colnames(series_ts), 
                       paste0(colnames(series_ts), "_def1"),
                       paste0(colnames(series_ts), "_def2"))
  
  # # Calculando a correlação entre as séries
  # corr_series = cor(series, method = "pearson")
  
  # Maiores correlações com a variavel resposta
  # num_var = 10
  if(!is.null(var_max)){
    if(var_max > (nrow(corr_series) - 1)){var_max = nrow(corr_series) - 1}else{
      var_max = var_max}}else{
        if((nrow(corr_series) - 1) < 15){var_max = nrow(corr_series) - 1}else{
          var_max = 15}
      }
  
  corr_resposta = corr_series[order(-corr_series[,1]), ][1:var_max]
  nomes_series = c(colnames(series_ts)[1], names(corr_resposta))
  series_def1 = select(data.frame(series), contains("def1"))
  series = series[,nomes_series]
  series_15mais = series
  
  # Atribuindo séries aos respectivos nomes das colunas
  for(k in 1:ncol(series)){
    assign(colnames(series)[k], series[,k])
  }
  
  for(k in 1:ncol(series_def1)){
    assign(colnames(series_def1)[k], series_def1[,k])
  }
  
  # Construindo formula do modelo
  formula_reg = paste(colnames(series)[1],  "~", colnames(series)[2], sep = " ")
  if(ncol(series) > 2){
    for(l in 3:ncol(series)){
      formula_reg = paste(formula_reg, " + ", colnames(series)[l])
    }
  }
  formula_reg = as.formula(formula_reg)
  
  # Rodando o modelo
  t1 = proc.time()
  ajuste_glmulti = glmulti(formula_reg, intercept = TRUE, level = 1, 
                           method = "h", crit = "AIC", 
                           plotty = FALSE, report = FALSE, fitfunction = "lm",
                           chunk = 1, chunks = 3, maxsize = cov_max)
  t2 = proc.time()
  t3 = (t2 - t1)[3]
  
  # If we were passed a progress update function, call it
  
  formula_final = as.formula(paste(ajuste_glmulti@formulas[[1]][2], ajuste_glmulti@formulas[[1]][1], ajuste_glmulti@formulas[[1]][3]))
  formula_aux = paste(formula_final[[2]], formula_final[[1]], 1)
  
  aux_formula = strsplit(as.character(formula_final[3]), " ")
  aux_formula = aux_formula[[1]][! aux_formula[[1]] %in% c("1", "+")]
  
  teste_def = regexpr('def2', aux_formula)
  for(j in 1:length(aux_formula)){
    if(teste_def[j] < 1){
      formula_aux = paste(formula_aux, "+", aux_formula[j])
    }else{
      formula_aux = paste(formula_aux, "+", 
                          paste0(substr(as.character(aux_formula[j]), 1, nchar(as.character(aux_formula[j]))-4), 
                                 "def1"), 
                          "+", aux_formula[j])
    }
  }
  
  modelo_final = lm(as.formula(formula_aux))
  aux_col = strsplit(formula_aux, split = " ") 
  
  # Saídas da função
  ano = as.integer(tsp(series_def2)[1])
  mes = as.integer((tsp(series_def2)[1]%%1)/0.083) + 1
  serie_y = series[,1]
  serie_reg = ts(modelo_final$fitted.values, start = c(ano, mes), frequency=12)
  
  if(sum(colnames(series_def1) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]) == 1){
    
    series_output = data.frame(series[,colnames(series) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]],
                               series_def1[,colnames(series_def1) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]])
    colnames(series_output) = c(colnames(series)[colnames(series) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]],
                                colnames(series_def1)[colnames(series_def1) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]])
    
  }else{
    
    series_output = data.frame(series[,colnames(series) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]],
                               series_def1[,colnames(series_def1) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]])
    
  }
  
  series = ts(series_output, start = c(ano, mes), frequency = 12)
  series_modelo = series[,aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]]
  
  output = list(modelo = formula_aux, 
                resumo_modelo = summary(modelo_final),
                residuos = modelo_final$residuals,
                serie_ajustada = serie_reg,
                serie_reg = serie_reg,
                series = series_aux,
                series_15mais = series_15mais,
                series_def1 = series_def1,
                series_modelo = series_modelo,
                tempo = paste(iconv("Tempo de execução:", from="UTF-8", to="LATIN1"), round(t3/60, 4), "minutos"),
                cov_max = cov_max,
                variacao = variacao,
                lm = modelo_final)
  
  
  return(output)
}

# # Refazendo o modelo
# novomodelo_function  = function(){
#   
#   teste = as.character(input$ipc_itens)
#   series_novomodelo = mp()$series_15mais[,which(!colnames(mp()$series_15mais) %in% teste)]
#   
#   for(k in 1:ncol(series_novomodelo)){
#     assign(colnames(series_novomodelo)[k], series_novomodelo[,k])
#   }
#   
#   for(j in 1:ncol(mp()$series_def1)){
#     assign(colnames(mp()$series_def1)[j], mp()$series_def1[,j])
#   }
#   
#   formula_novomodelo = paste(colnames(series_novomodelo)[1], "~", 
#                              paste(colnames(series_novomodelo)[2:(ncol(series_novomodelo) - 1)], collapse = " + "), 
#                              "+", colnames(series_novomodelo)[ncol(series_novomodelo)])
#   
#   cov_max = mp()$cov_max
#   ajustenovo_glmulti = glmulti(as.formula(formula_novomodelo), intercept = TRUE, level = 1, 
#                                method = "h", crit = "AIC", 
#                                plotty = FALSE, report = FALSE, fitfunction = "lm",
#                                chunk = 1, chunks = 3, maxsize = cov_max)
#   
#   formula_final = as.formula(paste(ajustenovo_glmulti@formulas[[1]][2], ajustenovo_glmulti@formulas[[1]][1], ajustenovo_glmulti@formulas[[1]][3]))
#   formula_aux = paste(formula_final[[2]], formula_final[[1]], 1)
#   formula_final
#   
#   aux_formula = strsplit(as.character(formula_final[3]), " ")
#   aux_formula = aux_formula[[1]][! aux_formula[[1]] %in% c("1", "+")]
#   
#   teste_def = regexpr('def2', aux_formula)
#   for(j in 1:length(aux_formula)){
#     if(teste_def[j] < 1){
#       formula_aux = paste(formula_aux, "+", aux_formula[j])
#     }else{
#       formula_aux = paste(formula_aux, "+", 
#                           paste0(substr(as.character(aux_formula[j]), 1, nchar(as.character(aux_formula[j]))-4), 
#                                  "def1"), 
#                           "+", aux_formula[j])
#     }
#   }
#   
#   modelo_final = lm(as.formula(formula_aux))
#   aux_col = strsplit(formula_aux, split = " ") 
#   
#   # Saídas da função
#   ano = year(as.Date(series_novomodelo)[1])
#   mes = month(as.Date(series_novomodelo)[1])
#   serie_y = series_novomodelo[,1]
#   serie_reg = ts(modelo_final$fitted.values, start = c(ano, mes), frequency=12)
#   
#   series_output = data.frame(series_novomodelo[,colnames(series_novomodelo) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]],
#                              mp()$series_def1[,colnames(mp()$series_def1) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]])
#   series = ts(series_output, start = c(ano, mes), frequency = 12)
#   series_modelo = series[,aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]]
#   
#   output = list(modelo = formula_aux, 
#                 resumo_modelo = summary(modelo_final),
#                 residuos = modelo_final$residuals,
#                 serie_ajustada = serie_reg,
#                 serie_reg = serie_reg,
#                 series_modelo = series_modelo,
#                 lm = modelo_final)
#   
# }


# S?rie temporal
regressao_parametrica = function(serie_original, defx, defy, covar=covar_aux, auto=auto_aux){
  
  covar_aux = c()
  auto_aux = TRUE
  
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
  
  # Per?odo das s?ries
  mes = as.numeric(substr(x = serie_original[1,1], start = 6, stop = 7))
  ano = as.numeric(substr(x = serie_original[1,1], start = 1, stop = 4))
  mes_acum = as.numeric(substr(x = serie_original[1,1], start = 6, stop = 7)) + 1
  
  # N?mero de covari?veis
  n_covar = ncol(serie_original) - 2
  
  # Varia??o do pre?o do transformador
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
      
      # Regress?o com defy defasagens para y e defx defasagens para x
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                            as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                            "1 months")),
        #                         ajuste_glmulti@objects[[1]]$fitted.values, 
        #                         ajuste_glmulti@objects[[1]]$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida\\Resultados_RegParam.xlsx",
        #                  sheetName = "Serie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                                  "1 months")),
        #                              ajuste_glmulti@objects[[1]]$fitted.values, 
        #                              ajuste_glmulti@objects[[1]]$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida\\Resultados_RegParam.xlsx",
        #                  sheetName = "Serie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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
      
      # Regress?o com defy defasagens para y e defx defasagens para x
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
        #ajuste_glmultifinal = ajuste_glmulti@formulas[[1]]
        #teste = lm(ajuste_glmultifinal)
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                                  "1 months")),
        #                              ajuste_glmulti@objects[[1]]$fitted.values, 
        #                              ajuste_glmulti@objects[[1]]$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida\\Resultados_RegParam.xlsx",
        #                  sheetName = "Serie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti@objects[[1]]$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti@objects[[1]]$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                                  "1 months")),
        #                              ajuste_glmulti@objects[[1]]$fitted.values, 
        #                              ajuste_glmulti@objects[[1]]$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida\\Resultados_RegParam.xlsx",
        #                  sheetName = "Serie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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
      
      # Regress?o com defy defasagens para y e defx defasagens para x
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                                  "1 months")),
        #                              ajuste_glmulti$fitted.values, 
        #                              ajuste_glmulti$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida\\Resultados_RegParam.xlsx",
        #                  sheetName = "Serie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                                  "1 months")),
        #                              ajuste_glmulti$fitted.values, 
        #                              ajuste_glmulti$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida\\Resultados_RegParam.xlsx",
        #                  sheetName = "Serie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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
      
      # Regress?o com defy defasagens para y e defx defasagens para x
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
        #ajuste_glmultifinal = ajuste_glmulti@formulas[[1]]
        #teste = lm(ajuste_glmultifinal)
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2012.8, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                                  "1 months")),
        #                              ajuste_glmulti$fitted.values, 
        #                              ajuste_glmulti$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida\\Resultados_RegParam.xlsx",
        #                  sheetName = "Serie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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
        #p1 = plot(serie_y, main=" ", xlab=" ", ylab=" ")
        #p2 = lines(serie_reg, col="blue", lty="dotted")
        #p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Regressao"), lty = c(1,2), lwd = 1,
        #            col = c("black", "blue"), cex = 0.7)
        #p1
        #p2
        #p3
        
        #ano_final = ano + length(ajuste_glmulti$fitted.values)%/%12
        #mes_final = mes_def + (length(ajuste_glmulti$fitted.values)%%12) - 1
        #series_regressao = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_def, "01", sep="/")),
        #                                                  as.Date(paste(ano_final, mes_final, "01", sep="/")),
        #                                                  "1 months")),
        #                              ajuste_glmulti$fitted.values, 
        #                              ajuste_glmulti$residuals)
        #colnames(series_regressao) = c("Per?odo", "Serie Ajustada", "Res?duos")
        #xlsx::write.xlsx( series_regressao,
        #                  file = "Saida/Resultados_RegParam.xlsx",
        #                  sheetName = "S?rie Ajustada",
        #                  row.names = FALSE,
        #                  col.names = TRUE)
        
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


# ?ndice fixo
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
  
  # Per?odo da s?rie
  mes_acum = as.numeric(substr(x = serie_original$Periodo[1], start = 6, stop = 7)) + 1
  ano = as.numeric(substr(x = serie_original$Periodo[1], start = 1, stop = 4))
  
  # Calcular a varia??o do pre?o do transformador
  var_Transf = NA
  for(k in 1:(length(serie_original[,2])-1)){
    var_Transf[k] = ((serie_original[k+1,2] - serie_original[k,2])/serie_original[k,2])*100
  }
  
  # Especificar ?nicio da s?ries acumulada
  mes_acum = as.numeric(substr(x = serie_original$Periodo[1], start = 6, stop = 7)) + 1
  
  preco_aux = matrix(NA, nrow(serie_original), length(coef))
  colnames(preco_aux) = names(serie_original)[3:(length(coef)+2)]
  #preco_aux[,1] = rep(coef[1], nrow(serie_original))
  for(i in 1:length(coef)){
    preco_aux[,i] = coef[i]*serie_original[,(covar_novo[i]+2)]
  }
  Preco_Parametrico = apply(preco_aux, 1, sum)
  PrecoAcum_Parametrico = cumsum(Preco_Parametrico)
  PrecoAcum_Parametrico = ts(PrecoAcum_Parametrico[-1], start = c(ano, mes_acum), frequency=12)
  
  Preco_Acum =  ts(cumsum(var_Transf), start = c(ano, mes_acum), frequency=12)
  
  coef_aux = matrix(coef, 1, length(coef))
  colnames(coef_aux) = colnames(serie_original)[(covar_novo+2)]
  
  #   p1 = plot(Preco_Acum, main=" ", xlab=" ", ylab=" ")
  #   p2 = lines(PrecoAcum_Parametrico, col="red", lty=2)
  #   p3 = legend(x=2013.2, y=15, legend = c("PT_acum", "PT_Parametrico"), lty = c(1,2), lwd = 1,
  #               col = c("black", "red"), cex = 0.7)
  #   
  #   ano_final = ano + length(Preco_Acum)%/%12
  #   mes_final = mes_acum + (length(Preco_Acum)%%12) - 1
  #   serie_indparam = data.frame(zoo::as.yearmon(seq(as.Date(paste(ano, mes_acum, "01", sep="/")),
  #                                                   as.Date(paste(ano_final, mes_final, "01", sep="/")),
  #                                                   "1 months")),
  #                               Preco_Acum, PrecoAcum_Parametrico)
  #   colnames(serie_indparam) = c("Período", "Preço Acumulado","Índice Paramétrico Acumulado")
  
  #   xlsx::write.xlsx( serie_indparam,
  #                     file = "Saida\\Resultados_IndFixo.xlsx",
  #                     sheetName = "Series",
  #                     row.names = FALSE,
  #                     col.names = TRUE)
  
  results = list(Preco_Parametrico = Preco_Parametrico, 
                 PrecoParametrico_acumulado = PrecoAcum_Parametrico, 
                 Preco_Transformador = Preco_Acum,
                 Coeficientes = coef_aux)
  return(results)
}


# auto.arima

auto_arima <- function(serie){ #d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2){
  
  auto <- auto.arima(serie)
  return(auto$arma)
  
}

# ARIMA <- function(serie, ordens = c()){ #d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2){
#   
#   Arima(serie, order = c(ordens[1],ordens[6],ordens[2]), 
#         seasonal = list(order = c(ordens[3],ordens[7],ordens[4]), period = 12))
#   
# }

# Função do teste t para o modelo ARIMA
t_test <- function(serie, modelo_arima, n_x = 0){
  
  # Esta funçãoo faz o teste de Significância dos parâmetros de um modelo ARIMA
  # n_x = o número de variáveis exógenas
  
  # Estatística T
  coef <- modelo_arima$coef
  se <- sqrt(diag(modelo_arima$var.coef))
  t <- abs(coef/se)
  # Teste t
  ok <- t > qt(0.975, length(serie) - sum(modelo_arima$arma[c(1,2,3,4,6,7)]) - n_x)
  ok[which(ok == TRUE)] <- "Sim"
  ok[which(ok == FALSE)] <- "Não"
  resul <- data.frame(Coefiente = coef, Desvio = se, t = t, Significativo = ok )
  return(resul)
  
}





# # Função de busca de informações das séries da FGV
# 
# encontrar_st <- function(palavra = NULL, codigo = NULL, print = FALSE){
#   
#   fgv_dados <- infos
#   
#   if( is.null(palavra) & is.null(codigo)){ stop("Informe a palavra ou o código da série")
#   }else{
#     
#     # testar se a busca é por código ou palavras
#     if(!is.null(codigo)){
#       masc <- fgv_dados[,"cod"] %in% codigo 
#       series  <- fgv_dados[masc,]
#       if(dim(series)[1] != 0){ print(series) 
#       }else{print(paste("O código", codigo, "não existe em nossa base de dados."))}
#     }else{
#       # Se a busca é por palavra...
#       # guardar a coluna
#       words <- casefold(palavra)
#       nomes <- casefold(fgv_dados[,"nome"])
#       # procurar palavra na coluna
#       masc <- grepl(words[1],nomes)
#       palavras <- palavra[1]
#       
#       if(length(palavra) > 1){
#         for(i in 2:length(palavra)){
#           masc <- masc & grepl(words[i],nomes)
#           palavras <- paste0(palavras,", ", palavra[i])
#         }
#       }
#       if(print){
#         print(paste0("Foram encontradas ", sum(masc), " séries com a(s) palavra(s): ", palavras))
#       } 
#       # Testar se encontrou algo 
#       if(sum(masc) != 0){
#         fgv_dados[masc,]            
#         
#       }
#     }
#   }
# }

