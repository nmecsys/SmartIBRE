shinyServer(function(input, output,session) {
  
#   # session$onSessionEnded(function() {
#   #   stopApp()
#   # })
#   # 
#   ### PÁGINA INICIAL ----------------------------------------- 
#   
#   output$home <- renderDygraph({
#     
#     # Gerar Gráfico
#     dygraph(igp, main = "IGP-DI (% a.m.) - FGV | IBRE" )%>%
#       dySeries("V1", label = "IGP-DI") %>%
#       dyAxis("y", valueRange = c(-4, 100)) %>%
#       dyAnnotation("1986-2-1", text = "Fev/86", tooltip = "Plano Cruzado", 
#                    tickHeight=40, width = 50, height=20) %>%
#       dyAnnotation("1986-11-1", text = "Nov/86", tooltip = "Plano Cruzado II", 
#                    tickHeight=50, width = 50, height=20) %>%
#       dyAnnotation("1987-7-1", text = "Jul/87", tooltip = "Plano Bresser", 
#                    tickHeight=80, width = 50, height=20) %>%
#       dyAnnotation("1989-1-1", text = "Jan/89", tooltip = "Plano Verão", 
#                    tickHeight=50, width = 50, height=20) %>%
#       dyAnnotation("1990-3-1", text = "Mar/90", tooltip = "Plano Collor", 
#                    tickHeight=20, width = 50, height=20) %>%
#       dyAnnotation("1991-1-1", text = "Jan/91", tooltip = "Plano Collor II", 
#                    tickHeight=40, width = 50, height=20) %>%
#       dyAnnotation("1994-7-1", text = "Jul/94", tooltip = "Real entra em circulação", 
#                    tickHeight=90, width = 50, height=20) %>%
#       dyAnnotation("1995-1-1", text = "Jan/95", tooltip = "Fernando Henrique Cardoso assume a presidência", 
#                    tickHeight=20, width = 50, height=20) %>%
#       dyAnnotation("1999-1-1", text = "Fev/99", tooltip = "Fernando Henrique Cardoso assume seu segundo mandato", 
#                    tickHeight=50, width = 50, height=20) %>%
#       dyAnnotation("2003-1-1", text = "Jan/03", tooltip = "Lula assume a presidência", 
#                    tickHeight=20, width = 50, height=20) %>%
#       dyAnnotation("2007-1-1", text = "Jan/07", tooltip = "Lula assume o segundo mandato", 
#                    tickHeight=50, width = 50, height=20) %>%
#       dyAnnotation("2008-9-1", text = "Set/08", tooltip = "Quebra do banco Lehman Brothers", 
#                    tickHeight=20, width = 50, height=20) %>%
#       dyAnnotation("2011-1-1", text = "Jan/11", tooltip = "Dilma Rousseff assume a presidência", 
#                    tickHeight=50, width = 50, height=20) %>%
#       dyAnnotation("2015-1-1", text = "Jan/15", tooltip = "Dilma Rousseff assume seu segundo mandato", 
#                    tickHeight=20, width = 50, height=20) %>%
#       dyOptions(fillGraph = TRUE, colors="dodgerblue", fillAlpha=0.3) %>%
#       dyRangeSelector(fillColor = "skyblue", dateWindow = c("1980-01-01", "2015-02-01"))
#   })
#   
#   
#   
#   ### INPUT -------------------------------------------------- 
#   
#   # Séries FGV ------------------ #
#   
#   menu_selecionado <- reactive({
#     input$menus
#   })
#   
#   output$a<- renderPrint({
#     menu_selecionado()
#     #input$ipa_indice
# 
#   })
#   
#   # Estrutura
#   ipa_estrutura <- reactive({
#     if(menu_selecionado() == "IPA-10"){
#       substr(input$ipa10_estrutura,nchar(input$ipa10_estrutura)-2,nchar(input$ipa10_estrutura)-1)
#     }else if(menu_selecionado() == "IPA-DI"){
#       substr(input$ipadi_estrutura,nchar(input$ipadi_estrutura)-2,nchar(input$ipadi_estrutura)-1)
#     }else if(menu_selecionado() == "IPA-M"){  
#       substr(input$ipam_estrutura,nchar(input$ipam_estrutura)-2,nchar(input$ipam_estrutura)-1)
#     }
#   })
#   
#   # selecionar estrutura OG ou EP
#   ipa_estrutura2 <- reactive({
#     ipa_estruturas_ok[input$ipa_estrutura == ipa_estruturas]
#   })
#   
#   # pegar unidade selecionada
#   ipa_unidade <- reactive({
#     ipa_unidades_ok[input$ipa_unidade == ipa_unidades]
#   })
#   
#   # filtrar por região e periodicidade
#   subset_ipa_novo <- reactive({
#     estrutura <- substr(subset_ipa$Serviço,6,7) == ipa_estrutura2()
#     periodo <- substr(subset_ipa$Serviço,9,10) == substr(input$ipa_indice,5,6)
#     unidade <- subset_ipa$Unidade == ipa_unidade()
#     subset_ipa[estrutura & periodo & unidade,]
#   })
#   
#   output$ipa_estruturaa <- renderPrint({
#     #head(subset_ipa_novo)
#     c(ipa_estrutura2(), ipa_unidade(), input$ipa_indice, dim(subset_ipa_novo()))
#     })
#   
#   
#   output$ipa10_estagios <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura())
#     nomes <- c("TODOS OS ESTÁGIOS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Estagio)), table(subset_ipa_estrutura$Estagio) != 0  & !(names(table(subset_ipa_estrutura$Estagio)) %in% c("NA 0", "NA NA")))) 
#     selectInput("ipa10_estagios", label = "1 - Estágios:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   output$ipa10_grupos <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() &  subset_ipa$Estagio == input$ipa10_estagios)
#     nomes <- c("TODOS OS GRUPOS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Grupo)), table(subset_ipa_estrutura$Grupo) != 0 & !(names(table(subset_ipa_estrutura$Grupo)) %in% c("NA 0", "NA NA")))) 
#     selectInput("ipa10_grupos", label = "2 - Grupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   output$ipa10_subgrupos <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() & subset_ipa$Estagio == input$ipa10_estagios & subset_ipa$Grupo == input$ipa10_grupos)
#     nomes <- c("TODOS OS SUBGRUPOS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Subgrupo)), table(subset_ipa_estrutura$Subgrupo) != 0 & !(names(table(subset_ipa_estrutura$Subgrupo)) %in% c("NA 0", "NA NA")))) 
#     selectInput("ipa10_subgrupos", label = "3 - Subgrupos:", width = "100%", 
#                 choices = nomes)
#   }) 
#   
#   output$ipa10_item <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() & subset_ipa$Estagio == input$ipa10_estagios & subset_ipa$Grupo == input$ipa10_grupos & subset_ipa$Subgrupo == input$ipa10_subgrupos)
#     nomes <- c("TODOS OS ITENS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Item)), table(subset_ipa_estrutura$Item) != 0 & !(names(table(subset_ipa_estrutura$Item)) %in% c('NA 0', 'NA NA')))) 
#     selectInput("ipa10_itens", label = "4 - Itens:", width = "100%", 
#                 choices = nomes)
#   }) 
#   
#   # IPA - DI  
#   output$ipadi_estagios <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura())
#     nomes <- c("TODOS OS ESTÁGIOS (MAIOR NÍVEL)",  subset(names(table(subset_ipa_estrutura$Estagio)), table(subset_ipa_estrutura$Estagio) != 0  & !(names(table(subset_ipa_estrutura$Estagio)) %in% c("NA 0", "NA NA"))))
#     selectInput("ipadi_estagios", label = "Estágios:", width = "50%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   })
#   
#   output$ipadi_grupos <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() &  subset_ipa$Estagio == input$ipadi_estagios)
#     nomes <- c("TODOS OS GRUPOS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Grupo)), table(subset_ipa_estrutura$Grupo) != 0 & !(names(table(subset_ipa_estrutura$Grupo)) %in% c("NA 0", "NA NA")))) 
#     selectInput("ipadi_grupos", label = "Grupos:", width = "65%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   })
#   
#   output$ipadi_subgrupos <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() & subset_ipa$Estagio == input$ipadi_estagios & subset_ipa$Grupo == input$ipadi_grupos)
#     nomes <- c("TODOS OS SUBGRUPOS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Subgrupo)), table(subset_ipa_estrutura$Subgrupo) != 0 & !(names(table(subset_ipa_estrutura$Subgrupo)) %in% c("NA 0", "NA NA")))) 
#     selectInput("ipadi_subgrupos", label = "Subgrupos:", width = "50%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   }) 
#   
#   output$ipadi_item <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() & subset_ipa$Estagio == input$ipadi_estagios & subset_ipa$Grupo == input$ipadi_grupos & subset_ipa$Subgrupo == input$ipadi_subgrupos)
#     nomes <- c("TODOS OS ITENS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Item)), table(subset_ipa_estrutura$Item) != 0 & !(names(table(subset_ipa_estrutura$Item)) %in% c('NA 0', 'NA NA')))) 
#     selectInput("ipadi_itens", label = "Itens:", width = "80%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   })
#   
#   # IPA - M  
#   output$ipam_estagios <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura())
#     nomes <- c("TODOS OS ESTÁGIOS (MAIOR NÍVEL)",  subset(names(table(subset_ipa_estrutura$Estagio)), table(subset_ipa_estrutura$Estagio) != 0  & !(names(table(subset_ipa_estrutura$Estagio)) %in% c("NA 0", "NA NA"))))
#     selectInput("ipam_estagios", label = "Estágios:", width = "50%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   })
#   
#   output$ipam_grupos <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() &  subset_ipa$Estagio == input$ipam_estagios)
#     nomes <- c("TODOS OS GRUPOS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Grupo)), table(subset_ipa_estrutura$Grupo) != 0 & !(names(table(subset_ipa_estrutura$Grupo)) %in% c("NA 0", "NA NA")))) 
#     selectInput("ipam_grupos", label = "Grupos:", width = "65%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   })
#   
#   output$ipam_subgrupos <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() & subset_ipa$Estagio == input$ipam_estagios & subset_ipa$Grupo == input$ipam_grupos)
#     nomes <- c("TODOS OS SUBGRUPOS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Subgrupo)), table(subset_ipa_estrutura$Subgrupo) != 0 & !(names(table(subset_ipa_estrutura$Subgrupo)) %in% c("NA 0", "NA NA")))) 
#     selectInput("ipam_subgrupos", label = "Subgrupos:", width = "50%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   }) 
# 
#   output$ipam_item <- renderUI({
#     subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura() & subset_ipa$Estagio == input$ipam_estagios & subset_ipa$Grupo == input$ipam_grupos & subset_ipa$Subgrupo == input$ipam_subgrupos)
#     nomes <- c("TODOS OS ITENS (MAIOR NÍVEL)", subset(names(table(subset_ipa_estrutura$Item)), table(subset_ipa_estrutura$Item) != 0 & !(names(table(subset_ipa_estrutura$Item)) %in% c('NA 0', 'NA NA')))) 
#     selectInput("ipam_itens", label = "Itens:", width = "80%", multiple=FALSE, selectize=FALSE,  size = 3,
#                 choices = nomes)
#   })
# 
#   # IPC
#   
#   # pegar sigla da região selecionada
#   ipc_regiao <- reactive({
#     ipc_regioes_siglas[input$ipc_regiao == ipc_regioes]
#   })
#   # pegar unidade selecionada
#   ipc_unidade <- reactive({
#     ipc_unidades_ok[input$ipc_unidade == ipc_unidades]
#   })
#   
#   # filtrar por região e periodicidade
#   subset_ipc_novo <- reactive({
#     regiao <- substr(subset_ipc$Serviço,4,5) == ipc_regiao()
#     periodo <- substr(subset_ipc$Serviço,9,10) == substr(input$ipc_indice,5,6)
#     unidade <- subset_ipc$Unidade == ipc_unidade()
#     subset_ipc[regiao & periodo & unidade,]
#   })
#   
#   output$teste10 <- renderPrint({
#     c(substr(input$ipc_indice,5,6))
#   })
#   
#   # mostrar grupos disponíveis
#   output$ipc_grupos <- renderUI({
#     novo_subset <- subset(subset_ipc_novo(), nchar(subset_ipc_novo()$item_fgv) == 1)
#     nomes_codigos <- ipc_estrutura[ipc_estrutura$Código %in% novo_subset$item_fgv,]
#     nomes_codigos <- paste(nomes_codigos$Código, nomes_codigos$Estrutura.Completa)
#     selectInput("ipc_grupos", label = "1 - Grupos:", width = "100%", choices = c('Selecione o grupo' ='', nomes_codigos))
#   })
#   
#   # mostrar subgrupos disponíveis
#   output$ipc_subgrupos <- renderUI({
#     grupo_selecionado <- substr(input$ipc_grupos,1,1)
#     novo_subset <- subset(subset_ipc_novo(), nchar(subset_ipc_novo()$item_fgv) == 2 & substr(subset_ipc_novo()$item_fgv,1,1) == grupo_selecionado)
#     nomes_codigos <- ipc_estrutura[ipc_estrutura$Código %in% novo_subset$item_fgv,]
#     nomes_codigos <- paste(nomes_codigos$Código, nomes_codigos$Estrutura.Completa)
#     selectInput("ipc_subgrupos", label = "2 - Subgrupos:", width = "100%", choices = c('Selecione o subgrupo' = '',nomes_codigos))
#   })
#   
#   # mostrar itens disponíveis
#   output$ipc_itens <- renderUI({
#     subgrupo_selecionado <- substr(input$ipc_subgrupos,1,2)
#     novo_subset <- subset(subset_ipc_novo(), nchar(subset_ipc_novo()$item_fgv) == 4 & substr(subset_ipc_novo()$item_fgv,1,2) == subgrupo_selecionado)
#     nomes_codigos <- ipc_estrutura[ipc_estrutura$Código %in% novo_subset$item_fgv,]
#     nomes_codigos <- paste(nomes_codigos$Código, nomes_codigos$Estrutura.Completa)
#     selectInput("ipc_itens", label = "3 - Itens:", width = "100%", choices = c('Selecione o item' = '',nomes_codigos))
#   })
#   
#   # mostrar subitens disponíveis
#   output$ipc_subitens <- renderUI({
#     item_selecionado <- substr(input$ipc_itens,1,4)
#     novo_subset <- subset(subset_ipc_novo(), nchar(subset_ipc_novo()$item_fgv) == 6 & substr(subset_ipc_novo()$item_fgv,1,4) == item_selecionado)
#     nomes_codigos <- ipc_estrutura[ipc_estrutura$Código %in% novo_subset$item_fgv,]
#     nomes_codigos <- paste(nomes_codigos$Código, nomes_codigos$Estrutura.Completa)
#     selectInput("ipc_subitens", label = "4 - Subitens:", width = "100%", choices = c('Selecione o subitem' = '',nomes_codigos))
#   })
#   
# 
#   output$tabelaipc <- renderPrint({
#     
#     c(input$ipc10_grupos, input$ipc10_itens)
#   })
#   
#   output$tabelaipc2 <- renderPrint({
#     menu_selecionado()
#   })
#   output$ipc_select <- renderPrint({ 
#     ipc_regiao10()
#   })
#   
#   
#   #########INCC ######################
#   
#   #INCC10--------------------------
#   
#   output$incc10_estagio <- renderUI({
#     #subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura())
#     nomes <- c("TODOS OS ESTÁGIOS (MAIOR NÍVEL)",unique(subset_incc$Estagio)) 
#     selectInput("incc10_estagios", label = "1 - Estágios:", width = "50%", 
#                 choices = nomes)
#   })
#   
#   
#   output$incc10_grupos <- renderUI({
#     subset_incc_estrutura <- subset(subset_incc,subset_incc$Estagio == input$incc10_estagios)
#     subset_incc_estrutura <- subset(subset_incc_estrutura,subset_incc_estrutura$Grupo !=0)
#     nomes <- c("TODOS OS GRUPOS (MAIOR NÍVEL)", unique(subset_incc_estrutura$Grupo)) 
#     selectInput("incc10_grupos", label = "2 - Grupos:", width = "60%", 
#                 choices = nomes)
#   })
#   
#   output$incc10_subgrupos <- renderUI({
#     subset_incc_grupos <- subset(subset_incc,subset_incc$Grupo == input$incc10_grupos)
#     subset_incc_grupos <- subset(subset_incc_grupos,subset_incc_grupos$Subgrupo!=0)
#     nomes <- c("TODOS OS SUBGRUPOS (MAIOR NÍVEL)", unique(subset_incc_grupos$Subgrupo)) 
#     selectInput("incc10_subgrupos", label = "3 - Subgrupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   output$incc10_itens <- renderUI({
#     subset_incc_subgrupos <- subset(subset_incc,subset_incc$Subgrupo == input$incc10_subgrupos)
#     subset_incc_subgrupos <- subset(subset_incc_subgrupos,subset_incc_subgrupos$Item!=0)
#     nomes <- c("TODOS OS Itens (MAIOR NÍVEL)", unique(subset_incc_subgrupos$Item)) 
#     selectInput("incc10_itens", label = "4 - Itens:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   
#   #########INCC ######################
#   
#   #inccm--------------------------
#   
#   output$inccm_estagio <- renderUI({
#     #subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura())
#     nomes <- c("TODOS OS ESTÁGIOS (MAIOR NÍVEL)",unique(subset_incc$Estagio)) 
#     selectInput("inccm_estagios", label = "1 - Estágios:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   
#   output$inccm_grupos <- renderUI({
#     subset_incc_estrutura <- subset(subset_incc,subset_incc$Estagio == input$inccm_estagios)
#     subset_incc_estrutura <- subset(subset_incc_estrutura,subset_incc_estrutura$Grupo !=0)
#     nomes <- c("TODOS OS GRUPOS (MAIOR NÍVEL)", unique(subset_incc_estrutura$Grupo)) 
#     selectInput("inccm_grupos", label = "2 - Grupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   output$inccm_subgrupos <- renderUI({
#     subset_incc_grupos <- subset(subset_incc,subset_incc$Grupo == input$inccm_grupos)
#     subset_incc_grupos <- subset(subset_incc_grupos,subset_incc_grupos$Subgrupo!=0)
#     nomes <- c("TODOS OS SUBGRUPOS (MAIOR NÍVEL)", unique(subset_incc_grupos$Subgrupo)) 
#     selectInput("inccm_subgrupos", label = "3 - Subgrupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   output$inccm_itens <- renderUI({
#     subset_incc_subgrupos <- subset(subset_incc,subset_incc$Subgrupo == input$inccm_subgrupos)
#     subset_incc_subgrupos <- subset(subset_incc_subgrupos,subset_incc_subgrupos$Item!=0)
#     nomes <- c("TODOS OS Itens (MAIOR NÍVEL)", unique(subset_incc_subgrupos$Item)) 
#     selectInput("inccm_itens", label = "4 - Itens:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   
#   #########INCC ######################
#   
#   #inccdi--------------------------
#   
#   output$inccdi_estagio <- renderUI({
#     #subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura())
#     nomes <- c("TODOS OS ESTÁGIOS (MAIOR NÍVEL)",unique(subset_incc$Estagio)) 
#     selectInput("inccdi_estagios", label = "1 - Estágios:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   
#   output$inccdi_grupos <- renderUI({
#     subset_incc_estrutura <- subset(subset_incc,subset_incc$Estagio == input$inccdi_estagios)
#     subset_incc_estrutura <- subset(subset_incc_estrutura,subset_incc_estrutura$Grupo !=0)
#     nomes <- c("TODOS OS GRUPOS (MAIOR NÍVEL)", unique(subset_incc_estrutura$Grupo)) 
#     selectInput("inccdi_grupos", label = "2 - Grupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   output$inccdi_subgrupos <- renderUI({
#     subset_incc_grupos <- subset(subset_incc,subset_incc$Grupo == input$inccdi_grupos)
#     subset_incc_grupos <- subset(subset_incc_grupos,subset_incc_grupos$Subgrupo!=0)
#     nomes <- c("TODOS OS SUBGRUPOS (MAIOR NÍVEL)", unique(subset_incc_grupos$Subgrupo)) 
#     selectInput("inccdi_subgrupos", label = "3 - Subgrupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   output$inccdi_itens <- renderUI({
#     subset_incc_subgrupos <- subset(subset_incc,subset_incc$Subgrupo == input$inccdi_subgrupos)
#     subset_incc_subgrupos <- subset(subset_incc_subgrupos,subset_incc_subgrupos$Item!=0)
#     nomes <- c("TODOS OS Itens (MAIOR NÍVEL)", unique(subset_incc_subgrupos$Item)) 
#     selectInput("inccdi_itens", label = "4 - Itens:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   
#   # Câmbio - Mundo
#   cambio_mundoInput <- reactive({
#     switch(input$mundo_cambio,
#            "Real (Brasil)" = "BRL=X",
#            "Boliviano (Bolivia)" = "BOB=X",
#            "Euro (Euro)" = "EUR=X",
#            "Guarani (Paraguai)" = "PYG=X",
#            "Novo Peso (Uruguai)" = "UYU=X",
#            "Peso (Argentina)" = "ARS=X",
#            "Peso (Chile)" = "CLP=X",
#            "Peso (Colombia)" = "COP=X",
#            "Peso (Cuba)" = "CUP=X",
#            "Peso (Mexico)" = "MXN=X",
#            "Yen (Japao)" = "JPY=X",
#            "Yuan (China)" = "CNY=X")
#   })
#   
#   # Séries Disponíveis para ajuste sazonal
#   sazonInput <- reactive({
#     switch(input$sazon,
#            "Selecione ST" = "selecione",
#            "Airline" = air,
#            "UK Driver Deaths" = UKDriverDeaths,
#            "IPC-DI (IBRE)" = ipc,
#            "PIM-PF (IBGE)" = pim)
#   })
#   
#   
#   # Importar um arquivo local no formato .csv para ajuste sazonal
#   importado_as <- reactive({
#     arquivo <- input$arquivo_local_as
#     
#     if (is.null(arquivo)){return(NULL)}else{
#       
#       dados <- read.csv2(arquivo$datapath)
#       mes_inicio <- as.numeric(substr(dados[1,1],4,5))
#       ano_inicio <- as.numeric(substr(dados[1,1],7,10))
#       ts <- ts(dados[,-1], start =  c(ano_inicio, mes_inicio), freq = 12)
#       nome <- substr(arquivo$name,1,nchar(arquivo$name)-4)
#       list(ts = ts, nome = nome)
#     }
#   })
#   
#   # Importar um arquivo local no formato .xlsx para modelo paramétrico
#   importado_mp <- reactive({
#     arquivo <- input$arquivo_local_mp
#     
#     if (is.null(arquivo)){return(NULL)}else{
#       dados <- read.xlsx(arquivo$datapath, sheetIndex = 1)
#       dados
#     }
#   })
#   
#   # Importar um arquivo local no formato .csv para previsão ARIMA
#   importado_arima <- reactive({
#     arquivo <- input$arquivo_local_arima
#     
#     if (is.null(arquivo)){return(NULL)}else{
#       
#       dados <- read.csv2(arquivo$datapath)
#       mes_inicio <- as.numeric(substr(dados[1,1],4,5))
#       ano_inicio <- as.numeric(substr(dados[1,1],7,10))
#       ts <- ts(dados[,-1], start =  c(ano_inicio, mes_inicio), freq = 12)
#       nome <- substr(arquivo$name,1,nchar(arquivo$name)-4)
#       list(ts = ts, nome = nome)
#     }
#   })
#   
#   # Verificar se foi carregado arquivo do ajuste sazonal
#   output$arquivo_carregado_as <- reactive({
#     return(!is.null(importado_as()$ts))
#   })
#   
#   outputOptions(output, 'arquivo_carregado_as', suspendWhenHidden=FALSE)
#   
#   # Verificar se foi carregado arquivo do modelo paramétrico
#   output$arquivo_carregado_mp <- reactive({
#     return(!is.null(importado_mp()))
#   })
#   outputOptions(output, 'arquivo_carregado_mp', suspendWhenHidden=FALSE)
#   
#   # Verificar se foi carregado arquivo da previsão ARIMA
#   output$arquivo_carregado_arima <- reactive({
#     return(!is.null(importado_arima()$ts))
#   })
#   outputOptions(output, 'arquivo_carregado_arima', suspendWhenHidden=FALSE)
#   
#   
#   ### OUTPUT -------------------------------------------------- 
#   
#   # Exportar resultados ajuste sazonal
#   formatoInput_as <- reactive({
#     switch(input$formato_as,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   
#   # Exportar resultados ajuste sazonal
#   formatoInput_as_imp <- reactive({
#     switch(input$formato_as_imp,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   # Exportar resultados modelo paramétrico
#   formatoInput_mp <- reactive({
#     switch(input$formato_mp,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   # Exportar resultados arima
#   formatoInput_arima <- reactive({
#     switch(input$formato_arima,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   # Exportar resultados  FGV: busca
#   formatoInput_fgv_busca<- reactive({
#     switch(input$formato_fgv_busca,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   # Exportar resultados  FGV: IGP
#   formatoInput_fgv_igp <- reactive({
#     switch(input$formato_fgv_igp,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   # Exportar resultados  FGV: IPA-10
#   formatoInput_fgv_ipa10 <- reactive({
#     switch(input$formato_fgv_ipa10,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   # Exportar resultados  FGV: IPA-DI
#   formatoInput_fgv_ipadi <- reactive({
#     switch(input$formato_fgv_ipadi,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   # Exportar resultados  FGV: IPA-M
#   formatoInput_fgv_ipam <- reactive({
#     switch(input$formato_fgv_ipam,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   
#   
#   
#   # Exportar resultados  FGV: INCC10
#   formatoInput_fgv_incc10 <- reactive({
#     switch(input$formato_fgv_incc10,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
# 
#   
#   # Exportar resultados  FGV: INCCDI
#   formatoInput_fgv_inccdi <- reactive({
#     switch(input$formato_fgv_inccdi,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   # Exportar resultados  FGV: INCCM
#   formatoInput_fgv_inccm <- reactive({
#     switch(input$formato_fgv_inccm,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   # Exportar resultados  FGV: IPC
#   formatoInput_fgv_ipc <- reactive({
#     switch(input$formato_fgv_ipc,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   
#   
#   
#   #Exportar resultados da industria
#   # Exportar resultados  FGV: IPC
#   formatoInput_fgv_industria <- reactive({
#     switch(input$formato_fgv_industria,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   #Exportar resultados da comercio
#   formatoInput_fgv_comercio <- reactive({
#     switch(input$formato_fgv_comercio,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   #Exportar resultados da servico
#   formatoInput_fgv_servico <- reactive({
#     switch(input$formato_fgv_servico,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   #Exportar resultados da consumidor
#   formatoInput_fgv_consumidor <- reactive({
#     switch(input$formato_fgv_consumidor,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   #Exportar resultados da construcao
#   formatoInput_fgv_construcao <- reactive({
#     switch(input$formato_fgv_construcao,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   
#   # Exportar resultados  FGV: Carrinho
#   formatoInput_fgv_favoritos<- reactive({
#     switch(input$formato_fgv_favoritos,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   # Exportar resultados  BRASIL: IBGE
#   formatoInput_IBGE<- reactive({
#     switch(input$formato_IBGE,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   # Exportar resultados  BRASIL: BACEN
#   formatoInput_bacen<- reactive({
#     switch(input$formato_bacen,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   })
#   
#   # Exportar resultados MUNDO: Mundo
#   formatoInput_stmundo <- reactive({
#     switch(input$formato_stmundo,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   # Exportar resultados MUNDO: EUA
#   formatoInput_stmundo_eua <- reactive({
#     switch(input$formato_stmundo_eua,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   
#   # Exportar resultados MUNDO: AL
#   formatoInput_stmundo_al <- reactive({
#     switch(input$formato_stmundo_al,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   
#   
#   # Exportar resultados MUNDO: AL
#   formatoInput_stmundo_europa <- reactive({
#     switch(input$formato_stmundo_europa,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   
#   # Exportar resultados MUNDO: AL
#   formatoInput_stmundo_asia <- reactive({
#     switch(input$formato_stmundo_asia,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
# 
#   # Botão de download para ajuste sazonal para as séries disponíveis
#   output$download_as <- downloadHandler(
#     filename = function() { paste0(input$sazon,"_ajustesazonal", formatoInput_as()) },
#     content = function(file) {
#       
#       frame <- data.frame(cbind(ajuste_disp()$data, sazonInput(), ajuste_disp()$ajustada,
#                                 ajuste_disp()$fatores, ajuste_disp()$tendencia, ajuste_disp()$irregular)
#       )
#       colnames(frame) = c("Data", "Observados", "Ajustados", "Fatores_sazonais",
#                           "Tendência", "Irregular")
#       
#       if(input$formato_as == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_as == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F)
#       }else if(input$formato_as == ".txt"){write.table(frame, file, row.names = F, quote = FALSE)
#       }else if(input$formato_as == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_as == ".spss"){foreign::write.foreign(frame, file,paste0(input$sazon, ".spss"), package = "SPSS")
#       }else if(input$formato_as == ".sas"){foreign::write.foreign(frame, file,paste0(input$sazon, ".sas"), package = "SAS", validvarname = "V7")                                       
#         
#         
#       }})
#   
#   # Botão de download para ajuste sazonal para a série importada
#   output$download_as_imp <- downloadHandler(
#     filename = function() { paste0(importado_as()$nome,"_ajustesazonal", formatoInput_as_imp()) },
#     content = function(file) {
#       
#       frame <- data.frame(cbind(ajuste_imp()$data, importado_as()$ts, ajuste_imp()$ajustada,
#                                 ajuste_imp()$fatores, ajuste_imp()$tendencia, ajuste_imp()$irregular)
#       )
#       colnames(frame) = c("Data", "Observados", "Ajustados", "Fatores_sazonais",
#                           "Tendência", "Irregular")
#       
#       if(input$formato_as_imp == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_as_imp == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F)
#       }else if(input$formato_as_imp == ".txt"){write.table(frame, file, row.names = F, quote = FALSE)
#       }else if(input$formato_as_imp == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_as_imp == ".spss"){foreign::write.foreign(frame, file,paste0(importado_as()$nome, ".spss"), package = "SPSS")
#       }else if(input$formato_as_imp == ".sas"){foreign::write.foreign(frame, file,paste0(importado_as()$nome, ".sas"), package = "SAS", validvarname = "V7")                                       
#       }
#     })
#   
#   # Botão de download para os resultados do modelo paramétrico
#   output$download_mp <- downloadHandler(
#     filename = function() { paste0("modelo_parametrico", formatoInput_mp()) },
#     content = function(file) {
#       
#       data <- as.character(seq(as.Date(paste0(start(mp()$serie_ajustada)[1],"/",start(mp()$serie_ajustada)[2],"/",1)),
#                                as.Date(paste0(end(mp()$serie_ajustada)[1],"/",end(mp()$serie_ajustada)[2],"/",1)),
#                                "1 months"))
#       
#       
#       frame <- data.frame(Data = data, valores_originais = c(mp()$preco_acumulado),
#                           valores_ajustados = c(mp()$serie_ajustada),
#                           residuos = c(mp()$residuos))
#       
#       if(input$formato_mp == ".csv"){ write.csv2(frame, file, row.names = F)
#       }else if(input$formato_mp == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F)
#       }else if(input$formato_mp == ".txt"){write.table(frame, file, row.names = F, quote = FALSE)
#       }else if(input$formato_mp == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_mp == ".spss"){foreign::write.foreign(frame, file,"modelo_parametrico.spss", package = "SPSS")
#       }else if(input$formato_mp == ".sas"){foreign::write.foreign(frame, file,"modelo_parametrico.sas", package = "SAS", validvarname = "V7")                                       
#                                            
#                                            
#       }})
#   
#   # Botão de download os resultados do modelo ARIMA
#   output$download_arima <- downloadHandler(
#     filename = function() { paste0(importado_arima()$nome, "_resultados", formatoInput_arima()) },
#     content = function(file) {
#       
#       frame <- data.frame(exportar_arima())
#       
#       if(input$formato_arima == ".csv"){ write.csv2(frame, file, row.names = F)
#       }else if(input$formato_arima == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F)
#       }else if(input$formato_arima == ".txt"){write.table(frame, file, row.names = F, quote = FALSE)
#       }else if(input$formato_arima == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_arima == ".spss"){foreign::write.foreign(frame, file,paste0("resultados_previsao.spss"), package = "SPSS")
#       }else if(input$formato_arima == ".sas"){foreign::write.foreign(frame, file,paste0(input$sazon, ".sas"), package = "SAS", validvarname = "V7")                                       
#                                               
#                                               
#       }})
#   
#   # Botão de download os resultados da busca de séries
#   output$download_fgv_busca <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_busca()) },
#     content = function(file) {
#       
#       dados <- codigos_busca()$dados
#       nome <- codigos_busca()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_busca == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_busca == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_busca == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_busca == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_busca == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames="busca", package = "SPSS")
#       }else if(input$formato_fgv_busca == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#                                                   
#                                                   
#       }})
#   
#   # Botão de download os resultados do fgv_igp
#   output$download_fgv_igp <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_igp()) },
#     content = function(file) {
#       
#       dados <- dados_igp()$dados
#       nome <- dados_igp()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_igp == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_igp == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_igp == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_igp == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_igp == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames="igp", package = "SPSS")
#       }else if(input$formato_fgv_igp == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")}
#   })
#   
#   # Botão de download os resultados do fgv_ipa10
#   output$download_fgv_ipa10 <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_ipa10()) },
#     content = function(file) {
#       
#       dados <- dados_ipa10()$dados
#       nome <- dados_ipa10()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_ipa10 == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_ipa10 == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_ipa10 == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_ipa10 == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_ipa10 == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_ipa10 == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
# 
#   
#   
#   # Botão de download os resultados do fgv_inccdi
#   output$download_fgv_inccdi <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_inccdi()) },
#     content = function(file) {
#       
#       dados <- dados_inccdi()$dados
#       nome <- dados_inccdi()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(formato_fgv_inccdi == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_inccdi == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_inccdi == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_inccdi == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_inccdi == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_inccdi== ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   
#   # Botão de download os resultados do fgv_inccdi
#   output$download_fgv_inccm <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_inccm()) },
#     content = function(file) {
#       
#       dados <- dados_inccm()$dados
#       nome <- dados_inccm()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_inccm == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_inccm == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_inccm == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_inccm == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_inccm == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_inccm== ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   # Botão de download os resultados do fgv_ipa10
#   output$download_fgv_incc10 <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_incc10()) },
#     content = function(file) {
#       
#       dados <- dados_incc10()$dados
#       nome <- dados_incc10()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_incc10 == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_incc10 == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_incc10 == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_incc10 == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_incc10 == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_incc10 == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   # Botão de download os resultados do industria
#   output$download_fgv_industria <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_industria()) },
#     content = function(file) {
#       
#       dados <- dados_industria()$dados
#       nome <- dados_industria()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_industria == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_industria == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_industria == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_industria == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_industria == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_industria == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   
#   # Botão de download os resultados do comercio
#   output$download_fgv_comercio <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_comercio()) },
#     content = function(file) {
#       
#       dados <- dados_comercio()$dados
#       nome <- dados_comercio()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_comercio == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_comercio == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_comercio == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_comercio == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_comercio == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_comercio == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   # Botão de download os resultados do servico
#   output$download_fgv_servico <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_servico()) },
#     content = function(file) {
#       
#       dados <- dados_servico()$dados
#       nome <- dados_servico()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_servico == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_servico == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_servico == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_servico == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_servico == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_servico == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   
#   # Botão de download os resultados do consumidor
#   output$download_fgv_consumidor <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_consumidor()) },
#     content = function(file) {
#       
#       dados <- dados_consumidor()$dados
#       nome <- dados_consumidor()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_consumidor == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_consumidor == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_consumidor == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_consumidor == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_consumidor == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_consumidor == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   # Botão de download os resultados do construcao
#   output$download_fgv_construcao <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_construcao()) },
#     content = function(file) {
#       
#       dados <- dados_construcao()$dados
#       nome <- dados_construcao()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_construcao == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_construcao == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_construcao == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_construcao == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_construcao == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_construcao == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#       }})
#   
#   
#   
#   # Botão de download os resultados do fgv_ipaDI
#   output$download_fgv_ipadi <- downloadHandler(
#     filename = function() { paste0("FGVDados_IPADI_",substr(Sys.time(),1,10),formatoInput_fgv_ipadi()) },
#     content = function(file) {
#       
#       dados <- dados_ipadi()$dados
#       nome <- dados_ipadi()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_ipadi == ".csv"){ write.csv2(frame, file, row.names = F)
#       }else if(input$formato_fgv_ipadi == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_ipadi == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_ipadi == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_ipadi == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_ipadi == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#                                                   
#                                                   
#       }})
#   
#   
#   # Botão de download os resultados do fgv_ipam
#   output$download_fgv_ipam <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_ipam()) },
#     content = function(file) {
#       
#       dados <- dados_ipam()$dados
#       nome <- dados_ipam()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_ipam == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_ipam == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_ipam == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";")
#       }else if(input$formato_fgv_ipam == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_ipam == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_ipam == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#                                                   
#                                                   
#       }})
#   
#   
#   # Botão de download os resultados do fgv_ipc
#   output$download_fgv_ipc <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_ipc()) },
#     content = function(file) {
#       
#       dados <- dados_ipc()$dados
#       nome <- dados_ipc()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_ipc == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_ipc == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_ipc == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";")
#       }else if(input$formato_fgv_ipc == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_ipc == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_ipc == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#         
#         
#       }})
#   
#   
#   
#   # Botão de download os resultados do fgv_ipc
#   output$download_fgv_ipc <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_ipc()) },
#     content = function(file) {
#       
#       dados <- dados_ipc()$dados
#       nome <- dados_ipc()$nome
#       
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
#       
#       frame <- data.frame(cbind(data, dados))
#       colnames(frame) <- c("Data",nome)
#       
#       if(input$formato_fgv_ipc == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_ipc == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_ipc == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";")
#       }else if(input$formato_fgv_ipc == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_ipc == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_ipc == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS", validvarname = "V7")                                       
#         
#         
#       }})
#   
#   
#   
#   # Botão de download os resultados do CARRINHO
#   output$download_fgv_favoritos <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_fgv_favoritos()) },
#     content = function(file) {
#       # 
#       # dados <- favoritos_completa()$ts
#       # nome <- favoritos_completa()$nome
#       # 
#       # data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#       #                          as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#       #                          "1 months"))
#       
#       
#       Data<-data.frame("Data"=favoritos_completa()$data)
#       ts<-favoritos_completa()$ts
#       names(ts)<-favoritos_completa()$nome
#       
#       #     names(ts)<-favoritos_completa()$nome
#       #     $ts$Periodo<-Data
#       
#       #ts<-ts[,c(2,1)]
#       
#       Data<-cbind(Data,ts)
#       if (ncol(Data)<5){
#         Data
#       }else{
#         Data[,c(1:5)]
#       }
#       
#       
#       frame <- Data
#       #colnames(frame) <- c("Data",names(ts))
#       
#       if(input$formato_fgv_favoritos == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_fgv_favoritos == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_fgv_favoritos == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_fgv_favoritos == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_fgv_favoritos == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_fgv_favoritos== ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")                                       
#                                                     
#                                                     
#       }})
#   
#   # Botão de download MUNDO: mundo
#   output$download_stmundo_eua <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_stmundo_eua()) },
#     content = function(file) {
#       
#       data <- index(visu_eua()$dados)
#       frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_eua()$dados))
#       colnames(frame)[2] <- visu_eua()$nome
#       
#       if(input$formato_stmundo_eua == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_stmundo_eua == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_stmundo_eua == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_stmundo_eua == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_stmundo_eua == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_stmundo_eua == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")                                        
#       }
#     })  
#   
#   # Botão de download MUNDO: al
#   output$download_stmundo_al <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_stmundo_al()) },
#     content = function(file) {
#       
#       data <- index(visu_al()$dados)
#       frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_al()$dados))
#       colnames(frame)[2] <- visu_al()$nome
#       
#       if(input$formato_stmundo_al == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_stmundo_al == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_stmundo_al == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_stmundo_al == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_stmundo_al == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_stmundo_al == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")                                        
#       }
#     })  
#   
#   # Botão de download MUNDO: al
#   output$download_stmundo_europa <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_stmundo_europa()) },
#     content = function(file) {
#       
#       data <- index(visu_europa()$dados)
#       frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_europa()$dados))
#       colnames(frame)[2] <- visu_europa()$nome
#       
#       if(input$formato_stmundo_europa == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_stmundo_europa == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_stmundo_europa == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_stmundo_europa == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_stmundo_europa == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_stmundo_europa == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")                                        
#       }
#     })  
#   
#   # Botão de download MUNDO: al
#   output$download_stmundo_asia <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_stmundo_asia()) },
#     content = function(file) {
#       
#       data <- index(visu_asia()$dados)
#       frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_asia()$dados))
#       colnames(frame)[2] <- visu_asia()$nome
#       
#       if(input$formato_stmundo_asia == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_stmundo_asia == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_stmundo_asia == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_stmundo_asia == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_stmundo_asia == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_stmundo_asia == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")                                        
#       }
#     })  
#   
#   # Botão de download MUNDO: mundo
#   output$download_stmundo <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_stmundo()) },
#     content = function(file) {
#       
#       data <- index(visu_mundo()$dados)
#       frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_mundo()$dados))
#       colnames(frame)[2] <- visu_mundo()$nome
#       
#       if(input$formato_stmundo == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_stmundo == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_stmundo == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_stmundo == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_stmundo == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_stmundo == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")                                        
#       }
#     })  
#   
#   # Botão de download IBGE
#   output$download_IBGE <- downloadHandler(
#     filename = function() { paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_IBGE()) },
#     content = function(file) {
#       
#       data <- as.Date(completo()$ts)
#       frame <- data.frame(Data = as.character(data), dados = completo()$ts)
#       
#       if(input$formato_IBGE == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_IBGE == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_IBGE == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_IBGE == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_IBGE == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_IBGE == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")                                        
#       }
#     })
#   
#   
#   
#   
#   
#   # Botão de download BACEN
#   output$download_BACEN <- downloadHandler(
# 
#     filename = function() {paste0("smartIBRE_",substr(Sys.time(),1,10),formatoInput_bacen())},
#     content = function(file) {
# 
#       dados <- dados_bacen()$ts
#       nome <- dados_bacen()$nome
# 
#       data <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                as.Date(paste0(end(dados)[1],"/",end(dados)[2],"/",1)),
#                                "1 months"))
# 
#       frame <- cbind(data, dados)
#       colnames(frame) <- c("Data",nome)
# 
#       #write.csv(frame, file, row.names = F)
# 
#       if(input$formato_bacen == ".csv"){ write.csv(frame, file, row.names = F)
#       }else if(input$formato_bacen == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F, showNA = F)
#       }else if(input$formato_bacen == ".txt"){write.table(frame, file, row.names = F, quote = FALSE, sep = ";", na ="")
#       }else if(input$formato_bacen == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_bacen == ".spss"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".spss"), varnames=nome, package = "SPSS")
#       }else if(input$formato_bacen == ".sas"){foreign::write.foreign(frame, file,paste0("smartIBRE_",substr(Sys.time(),1,10), ".sas"), package = "SAS")
#       }
#     })
#   
#   
#   # Ajuste Sazonal --------------------------------------------------
#   
#   # Especificações do ajuste sazonal ------------------ #
#   
#   metodo <- reactive({
#     switch(input$metodo_ajuste,
#            "SEATS" = "seats",
#            "X11" = "x11")
#   })
#   
#   metodo_imp <- reactive({
#     switch(input$metodo_ajuste_imp,
#            "SEATS" = "seats",
#            "X11" = "x11")
#   })
#   
#   transform <- reactive({
#     switch(input$transform,
#            "Automático" = "auto",
#            "Log" = "log",
#            "Raiz Quadrada" = "sqrt",
#            "Inversa" = "inverse",
#            "Nenhuma" = "none")
#   })
#   
#   outlier <- reactive({
#     switch(input$outlier,
#            "Sim" = "",
#            "Não" = NULL)
#   })
#   
#   reg.aic <- reactive({
#     switch(input$reg.aictest,
#            "Sim" = c("td","easter"),
#            "Não" = NULL)
#   })
#   
#   transform_imp <- reactive({
#     switch(input$transform_imp,
#            "Automático" = "auto",
#            "Log" = "log",
#            "Raiz Quadrada" = "sqrt",
#            "Inversa" = "inverse",
#            "Nenhuma" = "none")
#   })
#   
#   outlier_imp <- reactive({
#     switch(input$outlier_imp,
#            "Sim" = "",
#            "Não" = NULL)
#   })
#   
#   reg.aic_imp <- reactive({
#     switch(input$reg.aictest_imp,
#            "Sim" = c("td","easter"),
#            "Não" = NULL)
#   })
#   
#   #  Fazer o ajuste sazonal para as séries disponíveis ---------------------------------- #
#   ajuste_disp <- reactive({
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     if(sum(sazonInput() <= 0) > 0){
#       if(input$metodo_ajuste == "SEATS"){
#         prev <- try(seas(sazonInput(), outlier = outlier(), regression.aictest = reg.aic(), forecast.save = "fct", seats.appendfcst = "yes"),
#                     silent = T)
#         fatores <- prev$series$s16
#         prev_st <- prev$series$fct
#         prev_st <- window(prev_st, start = start(prev_st), end = c(end(prev_st)[1]-1, end(prev_st)[2]), freq = 12)
#         ajustada <- prev$series$s11
#         tendencia <- prev$series$s12
#         irregular <- prev$series$s13
#       }else{
#         prev <-  try(seas(sazonInput(), x11 = "", outlier = outlier(), regression.aictest = reg.aic(), forecast.save = "fct", x11.appendfcst = "yes"),
#                      silent = T)
#         fatores <- prev$series$d16
#         prev_st <- prev$series$fct
#         ajustada <- prev$series$d11
#         tendencia <- prev$series$d12
#         irregular <- prev$series$d13
#       }
#     }else{
#       if(input$metodo_ajuste == "SEATS"){
#         if(input$transform == "Automático"){
#           prev <- try(seas(sazonInput(), outlier = outlier(), regression.aictest = reg.aic(), forecast.save = "fct", seats.appendfcst = "yes"),
#                       silent = T)
#         }else{
#           prev <-  try(seas(sazonInput(), transform.function = transform(), outlier = outlier(), regression.aictest = reg.aic(), forecast.save = "fct", seats.appendfcst = "yes"),
#                        silent = T)
#         }
#         fatores <- prev$series$s16
#         prev_st <- prev$series$fct
#         prev_st <- window(prev_st, start = start(prev_st), end = c(end(prev_st)[1]-2, end(prev_st)[2]), freq = 12)
#         ajustada <- prev$series$s11
#         tendencia <- prev$series$s12
#         irregular <- prev$series$s13
#       }else{
#         if(input$transform == "Automático"){
#           prev <-  try(seas(sazonInput(), x11 = "", outlier = outlier(), regression.aictest = reg.aic(), forecast.save = "fct", x11.appendfcst = "yes"),
#                        silent = T)
#         }else{
#           prev <-  try(seas(sazonInput(), x11 = "", transform.function = transform(), outlier = outlier(), regression.aictest = reg.aic(), forecast.save = "fct", x11.appendfcst = "yes"),
#                        silent = T)
#         }
#         fatores <- prev$series$d16
#         prev_st <- prev$series$fct
#         ajustada <- prev$series$d11
#         tendencia <- prev$series$d12
#         irregular <- prev$series$d13
#       }
#     }
#     
#     colnames(prev_st) <- c("Estimativa", "inf", "sup")
#     st_sa <- window(final(prev), start = start(sazonInput()), end = end(sazonInput()), freq = 12)
#     tendencia <- window(tendencia, start = start(sazonInput()), end = end(sazonInput()), freq = 12)
#     irregular <- window(irregular, start = start(sazonInput()), end = end(sazonInput()), freq = 12)
#     
#     fatores_prev <- window(fatores, start = start(prev_st), end = end(prev_st), freq = 12)
#     fatores <- window(fatores, start = start(sazonInput()), end = end(sazonInput()), freq = 12)
#     data <- format(as.Date(sazonInput()), "%b/%y")
#     list(ajuste = prev, data = data, fatores = fatores, prev_fatores = fatores_prev, prev_ajustada = prev_st, ajustada = st_sa, tendencia = tendencia, irregular = irregular)
#     
#   })
#   
#   # Diagnósticos do ajuste sazonal para as séries disponíveis
#   output$ajuste_summary <- renderPrint({
#     summary(ajuste_disp()$ajuste)
#   }, width = 100)
#   
#   output$ajuste_qs <- renderPrint({
#     qs(ajuste_disp()$ajuste)
#   }, width = 100)
#   
#   # criar tabela com os dados originais e dessazonalizados para as séries disponíveis
#   output$tabela_disp <- renderTable({
#     data.frame(Data = as.character(ajuste_disp()$data),
#                Observados = sazonInput(),
#                Ajustados = ajuste_disp()$ajustada,
#                Fatores_sazonais = ajuste_disp()$fatores,
#                Tendência = ajuste_disp()$tendencia,
#                Irregular = ajuste_disp()$irregular, row.names = NULL)
#   })
#   
#   # previsão dos fatores sazonais e da série de interesse
#   output$ajuste_prev_fatores_disp <- renderDygraph({
#     dygraph(ajuste_disp()$prev_fatores, main = paste("Previsão dos fatores sazonais para", input$sazon)) %>%
#       dyOptions(colors = "dodgerblue") %>%
#       dySeries(label = "Fator sazonal",strokeWidth = 1, strokePattern = "dashed", drawPoints = TRUE, pointSize = 3)
#   })
#   
#   output$ajuste_prev_st_disp <- renderDygraph({
#     dygraph(ajuste_disp()$prev_ajustada, main = paste("Previsão de",input$sazon,"com ajuste sazonal")) %>%
#       dySeries(c("inf" ,"Estimativa","sup"), color = "dodgerblue", strokeWidth = 1, drawPoints = TRUE, pointSize = 3,strokePattern = "dashed")
#   })
#   
#   
#   # Gráfico dos dados originais e dessazonalizados para as séries disponíveis
#   output$ajuste_graf_disp <- renderDygraph({
#     
#     
#     dados <- cbind(Observados = sazonInput(),
#                    Ajustados = ajuste_disp()$ajustada)
#     
#     # encontrar as datas iniciais e finais da série temporal
#     mes_final <- end(sazonInput())[2]
#     ano_final <- end(sazonInput())[1]
#     mes_inicial <- start(sazonInput())[2]
#     ano_inicial <- start(sazonInput())[1]
#     
#     # Fazer o gráfico
#     dygraph(dados[,1:2], main = paste(input$sazon, "com ajuste sazonal por X13-ARIMA-SEATS")) %>%
#       dySeries("Observados", strokeWidth = 1, color = "black") %>%
#       dySeries("Ajustados", strokeWidth = 2, color = "dodgerblue") %>%
#       dyRangeSelector(fillColor = "skyblue",
#                       dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                      paste0(ano_final,"-",mes_final,"-01")))
#     
#   })
#   
#   #  Fazer o ajuste sazonal para a série importada -------------------------------------- #
#   ajuste_imp <- reactive({
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     
#     if(sum(importado_as()$ts <= 0) > 0){
#       if(input$metodo_ajuste_imp == "SEATS"){
#         prev <- try(seas(importado_as()$ts, outlier = outlier_imp(), regression.aictest = reg.aic_imp(), forecast.save = "fct", seats.appendfcst = "yes"),
#                     silent = T)
#         fatores <- prev$series$s16
#         prev_st <- prev$series$fct
#         ajustada <- prev$series$s11
#         tendencia <- prev$series$s12
#         irregular <- prev$series$s13
#       }else{
#         prev <-  try(seas(importado_as()$ts, x11 = "", outlier = outlier_imp(), regression.aictest = reg.aic_imp(), forecast.save = "fct", x11.appendfcst = "yes"),
#                      silent = T)
#         fatores <- prev$series$d16
#         prev_st <- prev$series$fct
#         ajustada <- prev$series$d11
#         tendencia <- prev$series$d12
#         irregular <- prev$series$d13
#       }
#     }else{
#       if(input$metodo_ajuste_imp == "SEATS"){
#         if(input$transform_imp == "Automático"){
#           prev <- try(seas(importado_as()$ts, outlier = outlier_imp(), regression.aictest = reg.aic_imp(), forecast.save = "fct", seats.appendfcst = "yes"),
#                       silent = T)
#         }else{
#           prev <-  try(seas(importado_as()$ts, transform.function = transform_imp(), outlier = outlier_imp(), regression.aictest = reg.aic_imp(), forecast.save = "fct", seats.appendfcst = "yes"),
#                        silent = T)
#         }
#         fatores <- prev$series$s16
#         prev_st <- prev$series$fct
#         prev_st <- window(prev_st, start = start(prev_st), end = c(end(prev_st)[1]-2, end(prev_st)[2]), freq = 12)
#         ajustada <- prev$series$s11
#         tendencia <- prev$series$s12
#         irregular <- prev$series$s13
#       }else{
#         if(input$transform_imp == "Automático"){
#           prev <-  try(seas(importado_as()$ts, x11 = "", outlier = outlier_imp(), regression.aictest = reg.aic_imp(), forecast.save = "fct", x11.appendfcst = "yes"),
#                        silent = T)
#         }else{
#           prev <-  try(seas(importado_as()$ts, x11 = "", transform.function = transform_imp(), outlier = outlier_imp(), regression.aictest = reg.aic_imp(), forecast.save = "fct", x11.appendfcst = "yes"),
#                        silent = T)
#         }
#         fatores <- prev$series$d16
#         prev_st <- prev$series$fct
#         ajustada <- prev$series$d11
#         tendencia <- prev$series$d12
#         irregular <- prev$series$d13
#       }
#     }
#     
#     colnames(prev_st) <- c("Estimativa", "inf", "sup")
#     st_sa <- window(final(prev), start = start(importado_as()$ts), end = end(importado_as()$ts), freq = 12)
#     tendencia <- window(tendencia, start = start(importado_as()$ts), end = end(importado_as()$ts), freq = 12)
#     irregular <- window(irregular, start = start(importado_as()$ts), end = end(importado_as()$ts), freq = 12)
#     
#     fatores_prev <- window(fatores, start = start(prev_st), end = end(prev_st), freq = 12)
#     fatores <- window(fatores, start = start(importado_as()$ts), end = end(importado_as()$ts), freq = 12)
#     data <- format(as.Date(importado_as()$ts), "%b/%y")
#     list(ajuste = prev, data = data, fatores = fatores, prev_fatores = fatores_prev, prev_ajustada = prev_st, ajustada = st_sa, tendencia = tendencia, irregular = irregular)
#     
#   })
#   
#   # Diagnósticos do ajuste sazonal para a série importada
#   output$ajuste_imp_summary <- renderPrint({
#     summary(ajuste_imp()$ajuste)
#   }, width = 100)
#   
#   output$ajuste_imp_qs <- renderPrint({
#     qs(ajuste_imp()$ajuste)
#   }, width = 100)
#   
#   # criar tabela com os dados originais e dessazonalizados para a série importada
#   output$tabela_imp <- renderTable({
#     data.frame(Data = as.character(ajuste_imp()$data),
#                Observados = importado_as()$ts,
#                Ajustados = ajuste_imp()$ajustada,
#                Fatores_sazonais = ajuste_imp()$fatores,
#                Tendência = ajuste_imp()$tendencia,
#                Irregular = ajuste_imp()$irregular, row.names = NULL)
#   })
#   
#   # previsão dos fatores sazonais e da série de interesse
#   output$ajuste_prev_fatores_imp <- renderDygraph({
#     dygraph(ajuste_imp()$prev_fatores, main = paste("Previsão dos fatores sazonais para", importado_as()$nome)) %>%
#       dyOptions(colors = "dodgerblue") %>%
#       dySeries(label = "Fator sazonal",strokeWidth = 1, strokePattern = "dashed", drawPoints = TRUE, pointSize = 3)
#   })
#   
#   output$ajuste_prev_st_imp <- renderDygraph({
#     dygraph(ajuste_imp()$prev_ajustada, main = paste("Previsão de",importado_as()$nome,"com ajuste sazonal")) %>%
#       dySeries(c("inf" ,"Estimativa","sup"), color = "dodgerblue", strokeWidth = 1, drawPoints = TRUE, pointSize = 3,strokePattern = "dashed")
#   })
#   
#   
#   # Gráfico dos dados originais e dessazonalizados para a série importada
#   output$ajuste_graf_imp <- renderDygraph({
#     
#     dados <- cbind(Observados = importado_as()$ts,
#                    Ajustados = ajuste_imp()$ajustada)
#     
#     # encontrar as datas iniciais e finais da série temporal
#     mes_final <- end(importado_as()$ts)[2]
#     ano_final <- end(importado_as()$ts)[1]
#     mes_inicial <- start(importado_as()$ts)[2]
#     ano_inicial <- start(importado_as()$ts)[1]
#     
#     # Fazer o gráfico
#     dygraph(dados[,1:2], main = paste(importado_as()$nome, "com ajuste sazonal por X13-ARIMA-SEATS")) %>%
#       dySeries("Observados", strokeWidth = 1, color = "black") %>%
#       dySeries("Ajustados", strokeWidth = 2, color = "dodgerblue") %>%
#       dyRangeSelector(fillColor = "skyblue",
#                       dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                      paste0(ano_final,"-",mes_final,"-01")))
#     
#   })
#   
#   # Importar um arquivo local no formato .xlsx para modelo paramétrico
#   importado_mp2 <- reactive({
#     arquivo2 <- input$arquivo_local_mp2
#     
#     if (is.null(arquivo2)){return(NULL)}else{
#       dados2 <- read.xlsx(arquivo2$datapath, sheetIndex = 1)
#       dados2
#     }
#   })
#   
#   
#   # Verificar se foi carregado arquivo do modelo paramétrico
#   output$arquivo_carregado_mp2 <- reactive({
#     return(!is.null(importado_mp2()))
#   })
#   outputOptions(output, 'arquivo_carregado_mp2', suspendWhenHidden=FALSE)
#   
#   
#   # Checkboxes de entrada
#   
#   # INCC
#   output$choose_incc <- renderUI({
#     
#     if(input$incc){
#       aux_incc = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "INCC/E-DI"),]
#       grupos_incc = sort(as.character(unique(aux_incc$Grupo))[which(!as.character(unique(aux_incc$Grupo)) %in% c("#N/D", "-"))])
#       aux_lista = list("inccgrupo1")
#       for(k in 2:length(grupos_incc)){
#         aux_lista = c(aux_lista, paste0("inccgrupo", k))
#       }
#       names(aux_lista) = grupos_incc
#       checkboxGroupInput("grupos_incc", label = " ", width = "100%", choices = aux_lista)
#     }
#   })
#   
#   # Artifício para corrigir o problema que ocorre quando selecionamos "Mão de Obra" e "Materiais, Equipamentos e Serviços"
#   output$nav <- reactive({
#     if(length(input$grupos_incc) >= 2){"sim"}
#   })
#   outputOptions(output, 'nav', suspendWhenHidden = FALSE)
#   
#   # Terceiro nível na hierarquia do INCC
#   output$choose_incc2 <- renderUI({
#     
#     aux_incc = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "INCC/E-DI"),]
#     grupos_incc = sort(as.character(unique(aux_incc$Grupo))[which(!as.character(unique(aux_incc$Grupo)) %in% c("#N/D", "-"))])
#     aux_lista = list("inccgrupo1")
#     for(k in 2:length(grupos_incc)){
#       aux_lista = c(aux_lista, paste0("inccgrupo", k))
#     }
#     names(aux_lista) = grupos_incc
#     
#     teste_nome = names(aux_lista[aux_lista == "inccgrupo2"])
#     aux_incc2 = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "INCC/E-DI" &
#                                              fgvinformacao_grupos$Grupo %in% teste_nome),]
#     grupos_incc2 = sort(as.character(unique(aux_incc2$Subgrupo))[which(!as.character(unique(aux_incc2$Subgrupo)) %in% c("#N/D", "-"))])
#     
#     if("inccgrupo2" %in% input$grupos_incc){
#       checkboxGroupInput("grupos_incc2", label = " ", width = "100%", choices = grupos_incc2)
#     }
#     
#   })
#   
#   
#   # IPA
#   output$choose_ipa <- renderUI({
#     
#     if(input$ipa){
#       aux_ipa = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "IPA08OG-DI"),]
#       grupos_ipa = sort(as.character(unique(aux_ipa$Subgrupo))[which(!as.character(unique(aux_ipa$Subgrupo)) %in% c("#N/D", "-"))])
#       checkboxGroupInput("grupos_ipa", label = " ", width = "100%", choices = grupos_ipa)
#     }
#   })
#   
#   
#   # IPC
#   output$choose_ipc <- renderPrint({
#     
#     if(input$ipc){
#       aux_ipc = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "IPCBR12/DI"),]
#       grupos_ipc = sort(as.character(unique(aux_ipc$Grupo))[which(!as.character(unique(aux_ipc$Grupo)) %in% c("#N/D", "-"))])
#       checkboxGroupInput("grupos_ipc", label = " ", width = "100%", choices = grupos_ipc)
#     }
#     
#   })
#   
#   
#   # Exportar resultados 
#   formatoInput_mp <- reactive({
#     switch(input$formato_mp,
#            ".csv"  = ".csv",
#            ".xlsx" = ".xlsx",
#            ".txt" = ".txt",
#            ".dta" = ".dta",
#            ".sas" = ".sas",
#            ".spss" = ".spss")
#   }) 
#   
#   output$download_mp <- downloadHandler(
#     filename = function() { paste0("modelo_parametrico", formatoInput_mp()) },
#     content = function(file) {
#       
#       if(ntext()$var == "Sim"){variacao_aux = TRUE}else{variacao_aux = FALSE}
#       if(is.null(ntext()$variaveis)){
#         
#         if(variacao_aux == TRUE){
#           
#           aux_p1 = as.ts(cbind(Custo_Observado = mp_series()$series_modelo[,1], 
#                                Custo_Ajustado = mp_series()$serie_ajustada))
#           # data_aux = as.Date(aux_p1)
#           
#           data <- as.character(seq(as.Date(paste0(start(mp_series()$serie_ajustada)[1],"/",start(mp_series()$serie_ajustada)[2],"/",1)),
#                                    as.Date(paste0(end(mp_series()$serie_ajustada)[1],"/",end(mp_series()$serie_ajustada)[2],"/",1)),
#                                    "1 months"))
#           
#           frame <- data.frame(Data = data, 
#                               valores_originais = c(mp_series()$series_modelo[,1]),
#                               valores_ajustados = c(mp_series()$serie_ajustada),
#                               residuos = c(mp_series()$residuos))
#           
#         }else{
#           
#           inicio = nrow(mp_series()$series) - length(mp_series()$serie_ajustada)
#           
#           aux1 = mp_series()$series[inicio:(nrow(mp_series()$series) - 1), 2]
#           aux2 = ((mp_series()$serie_ajustada/100) + 1)*aux1
#           
#           ano = as.integer(tsp(mp_series()$series_modelo)[1])
#           mes = as.integer((tsp(mp_series()$series_modelo)[1]%%1)/0.083) + 1
#           
#           serie_ajustada = ts(aux2, start = c(ano, mes), freq = 12)
#           serie_original = ts(mp_series()$series[(inicio + 1):nrow(mp_series()$series), 2],
#                               start = c(ano, mes), freq = 12)
#           
#           # data_aux = as.Date(serie_original)
#           
#           aux_p1 = as.ts(cbind(Custo_Observado = serie_original,
#                                Custo_Ajustado = serie_ajustada))
#           
#           data <- as.character(seq(as.Date(paste0(start(serie_ajustada)[1],"/",start(serie_ajustada)[2],"/",1)),
#                                    as.Date(paste0(end(serie_ajustada)[1],"/",end(serie_ajustada)[2],"/",1)),
#                                    "1 months"))
#           
#           frame <- data.frame(Data = data, 
#                               valores_originais = c(serie_original),
#                               valores_ajustados = c(serie_ajustada),
#                               residuos = c(serie_ajustada - serie_original))
#         }
#       }else{
#         
#         if(variacao_aux == TRUE){
#           
#           aux_p1 = as.ts(cbind(Preco_Transf = novomodelo_reactive_series()$series_modelo[,1],
#                                Preco_Ajustado = novomodelo_reactive_series()$serie_ajustada))
#           
#           # data_aux = as.Date(aux_p1)
#           
#           data <- as.character(seq(as.Date(paste0(start(novomodelo_reactive_series()$serie_ajustada)[1],"/",start(novomodelo_reactive_series()$serie_ajustada)[2],"/",1)),
#                                    as.Date(paste0(end(novomodelo_reactive_series()$serie_ajustada)[1],"/",end(novomodelo_reactive_series()$serie_ajustada)[2],"/",1)),
#                                    "1 months"))
#           
#           frame <- data.frame(Data = data, 
#                               valores_originais = c(novomodelo_reactive_series()$series_modelo[,1]),
#                               valores_ajustados = c(novomodelo_reactive_series()$serie_ajustada),
#                               residuos = c(novomodelo_reactive_series()$residuos))
#           
#         }else{
#           
#           inicio = nrow(mp_series()$series) - length(novomodelo_reactive_series()$serie_ajustada)
#           
#           aux1 = mp_series()$series[inicio:(nrow(mp_series()$series) - 1), 2]
#           aux2 = ((novomodelo_reactive_series()$serie_ajustada/100) + 1)*aux1
#           
#           ano = as.integer(tsp(mp_series()$series_modelo)[1])
#           mes = as.integer((tsp(mp_series()$series_modelo)[1]%%1)/0.083) + 1
#           
#           serie_ajustada = ts(aux2, start = c(ano, mes), freq = 12)
#           serie_original = ts(mp_series()$series[(inicio + 1):nrow(mp_series()$series), 2],
#                               start = c(ano, mes), freq = 12)
#           
#           # data_aux = as.Date(serie_original)
#           
#           # aux_p1 = as.ts(cbind(Preco_Transf = serie_original,
#           #                      Preco_Ajustado = serie_ajustada))
#           
#           data <- as.character(seq(as.Date(paste0(start(serie_ajustada)[1],"/",start(serie_ajustada)[2],"/",1)),
#                                    as.Date(paste0(end(serie_ajustada)[1],"/",end(serie_ajustada)[2],"/",1)),
#                                    "1 months"))
#           
#           frame <- data.frame(Data = data, 
#                               valores_originais = c(serie_original),
#                               valores_ajustados = c(serie_ajustada),
#                               residuos = c(serie_ajustada - serie_original))
#           
#         }
#       }
#       
#       if(input$formato_mp == ".csv"){write.csv2(frame, file, row.names = F)
#       }else if(input$formato_mp == ".xlsx"){xlsx::write.xlsx(frame, file, row.names = F)
#       }else if(input$formato_mp == ".txt"){write.table(frame, file, row.names = F, quote = FALSE)
#       }else if(input$formato_mp == ".dta"){foreign::write.dta(frame, file)
#       }else if(input$formato_mp == ".spss"){foreign::write.foreign(frame, file,"modelo_parametrico.spss", package = "SPSS")
#       }else if(input$formato_mp == ".sas"){foreign::write.foreign(frame, file,"modelo_parametrico.sas", package = "SAS")                                       
#         
#       }
#     })
#   
#   output$download_dadosmodelo <- downloadHandler(
#     filename = function() { paste0("dados_mp3", formatoInput_mp()) },
#     content = function(file) {
#       
#       if(input$formato_mp == ".csv"){ write.csv2(dados_mp3(), file, row.names = F)
#       }else if(input$formato_mp == ".xlsx"){xlsx::write.xlsx(dados_mp3(), file, row.names = F)
#       }else if(input$formato_mp == ".txt"){write.table(dados_mp3(), file, row.names = F, quote = FALSE)
#       }else if(input$formato_mp == ".dta"){foreign::write.dta(dados_mp3(), file)
#       }else if(input$formato_mp == ".spss"){foreign::write.foreign(dados_mp3(), file,"modelo_parametrico.spss", package = "SPSS")
#       }else if(input$formato_mp == ".sas"){foreign::write.foreign(dados_mp3(), file,"modelo_parametrico.sas", package = "SAS")                                       
#         
#       }})
#   
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste('data-', Sys.Date(), '.csv', sep='')
#     },
#     content = function(con) {
#       write.csv(importado_mp2(), con)
#     }
#   )  
#   
#   # Modelo
#   
#   # Construir data frame baseado nas séries escolhidas
#   
#   # IPA
#   checkbox_ipa <- reactive({
#     
#     if(input$ipa){select_ipa = "-"
#     dados_ipa = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "IPA08OG-DI" & 
#                                              fgvinformacao_grupos$Grupo == select_ipa),]}
#     
#     subgrupos_ipa = input$grupos_ipa
#     dados_ipa = rbind(dados_ipa, fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "IPA08OG-DI" & 
#                                                               fgvinformacao_grupos$Subgrupo %in% subgrupos_ipa),])
#     
#     grupos_ipa = unique(dados_ipa$Grupo)
#     grupos_ipa = grupos_ipa[grupos_ipa != "-"]
#     
#     dados_ipa = rbind(dados_ipa, fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "IPA08OG-DI" & 
#                                                               fgvinformacao_grupos$Grupo %in% grupos_ipa & 
#                                                               fgvinformacao_grupos$Subgrupo == "-"),])
#     
#     dados_ipa
#   })
#   
#   # INCC
#   checkbox_incc <- reactive({
#     if(input$incc){select_incc = "-"
#     dados_incc = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "INCC/E-DI" & 
#                                               fgvinformacao_grupos$Grupo == select_incc),]}
#     if("inccgrupo1" %in% input$grupos_incc){dados_incc = rbind(dados_incc, fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "INCC/E-DI" & 
#                                                                                                         fgvinformacao_grupos$Grupo == "Mão de obra"),])}
#     if("inccgrupo2" %in% input$grupos_incc){dados_incc = rbind(dados_incc, fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "INCC/E-DI" & 
#                                                                                                         fgvinformacao_grupos$Grupo == "Materiais,  equipamentos e serviços" & 
#                                                                                                         fgvinformacao_grupos$Subgrupo %in% c("-", input$grupos_incc2)),])}
#     dados_incc
#   })
#   
#   # IPC
#   checkbox_ipc <- reactive({
#     
#     if(input$ipc){select_ipc = "-"
#     dados_ipc = fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "IPCBR12/DI" & 
#                                              fgvinformacao_grupos$Grupo == select_ipc),]}
#     
#     grupos_ipc = input$grupos_ipc
#     dados_ipc = rbind(dados_ipc, fgvinformacao_grupos[which(fgvinformacao_grupos$Serviço == "IPCBR12/DI" & 
#                                                               fgvinformacao_grupos$Grupo %in% grupos_ipc),])
#     dados_ipc
#   })
#   
#   # Dados Final
#   dados_modelo <- reactive({
#     
#     dados_modelo = matrix(NA, 0, ncol(fgvinformacao_grupos))
#     colnames(dados_modelo) = colnames(fgvinformacao_grupos)
#     if(input$todas_as_series){
#       
#       dados_modelo = fgvinformacao_grupos
#       
#     }else{
#       
#       if(input$ipa){
#         dados_modelo = rbind(dados_modelo, checkbox_ipa())}
#       if(input$incc){
#         dados_modelo = rbind(dados_modelo, checkbox_incc())}
#       if(input$ipc){
#         dados_modelo = rbind(dados_modelo, checkbox_ipc())}
#       
#     }
#     
#     dados_modelo$Série = paste0("ST_", dados_modelo$Série)
#     fgvaux_atualizado = fgv_aux[which(fgv_aux$Código %in% dados_modelo$Série),]
#     fgvdados_atualizado = fgv_dados[,which(colnames(fgv_dados) %in% fgvaux_atualizado$Descricao_Completa)]
#     fgvdados_atualizado
#   })
#   
#   # Criar data.frame para o modelo paramétrico
#   dados_mp3 <- reactive({
#     
#     teste_mp3 = importado_mp2()
#     
#     
#     aux_inicio_dadosmp2 <- max("200201", paste0(substr(teste_mp3[1,1], 1, 4), substr(teste_mp3[1,1], 6, 7)))
#     aux_final_dadosmp2 <- min("201504", paste0(substr(teste_mp3[nrow(teste_mp3),1], 1, 4), substr(teste_mp3[nrow(teste_mp3),1], 6, 7)))
#     
#     mes_inicio_dadosmp2 <- as.numeric(substr(aux_inicio_dadosmp2, 5, 7))
#     ano_inicio_dadosmp2 <- as.numeric(substr(aux_inicio_dadosmp2, 1, 4))
#     mes_final_dadosmp2 <- as.numeric(substr(aux_final_dadosmp2, 5, 7))
#     ano_final_dadosmp2 <- as.numeric(substr(aux_final_dadosmp2, 1, 4))
#     
#     fgv_dados2 = window(dados_modelo(), start = c(ano_inicio_dadosmp2, mes_inicio_dadosmp2),
#                         end = c(ano_final_dadosmp2, mes_final_dadosmp2))
#     
#     mes_dados = month(teste_mp3[1,1])
#     ano_dados = year(teste_mp3[1,1])
#     dados_ts = ts(teste_mp3[,2], start = c(ano_dados, mes_dados), freq = 12)
#     dados_ts2 = window(dados_ts, start = c(ano_inicio_dadosmp2, mes_inicio_dadosmp2),
#                        end = c(ano_final_dadosmp2, mes_final_dadosmp2))
#     
#     # Essa lista faz com que o data.frame apareça com os nomes corretos nas colunas caso somente um índice seja selecionado
#     lista_input = data.frame(nomes = c("INCC_Todos_os_Itens", "IPA_Todos_os_Itens", "IPC_Todos_os_Itens"),
#                              condicao = c(input$incc, input$ipa, input$ipc))
#     
#     series = data.frame(Periodo = as.Date(dados_ts2),
#                         Preco = dados_ts2,
#                         fgv_dados2)
#     
#     if(ncol(series) == 3){
#       colnames(series)[3] = as.character(lista_input[which(lista_input$condicao == TRUE), "nomes"])
#     }
#     
#     aux = apply(apply(series, 2, is.na), 2, sum)
#     series2 = series[, names(aux[aux == 0])]
#     
#     series2
#   })
#   
#   # Identificar se as covariáveis foram escolhidas ou não - AINDA NÃO ESTÁ CORRETO!!
#   ntext <- eventReactive(input$atualizar2, {
#     
#     if(is.null(input$ipc_itens)){
#       list(cov_num = input$cov_max, var = input$variacao)
#     }else{list(cov_num = input$cov_max, var = input$variacao, variaveis = unlist(checkbox_series()))}
#     
#   })
#   
#   # 
#   output$teste_dadosmp3 <- renderPrint({
#     
#     dim(dados_mp3())
#     
#   })
#   
#   
#   output$teste_eventReactive <- renderPrint({
#     
#     nText()
#     
#   })
#   
#   # Acumular as variáveis excluídas
#   nText <- reactive({
#     teste <<- c(teste, ntext()$variaveis)
#     teste[-1]
#   })
#   
#   # Primeira etapa do modelo (sem excluir nenhuma variável)
#   mp_series <- reactive({
#     input$atualizar2
#     if(ntext()$var == "Sim"){variacao_aux = TRUE}else{variacao_aux = FALSE}
#     isolate(cluster_function(series = dados_mp3(), cov_max = ntext()$cov_num, var_max = 15, variacao = variacao_aux))
#   }) 
#   
#   
#   # Modelo novo (excluindo covariáveis)
#   novomodelo_reactive_series <- reactive({
#     
#     series_novomodelo = mp_series()$series_15mais[,which(!colnames(mp_series()$series_15mais) %in% nText())]
#     
#     for(k in 1:ncol(series_novomodelo)){
#       assign(colnames(series_novomodelo)[k], series_novomodelo[,k])
#     }
#     
#     for(j in 1:ncol(mp_series()$series_def1)){
#       assign(colnames(mp_series()$series_def1)[j], mp_series()$series_def1[,j])
#     }
#     
#     formula_novomodelo = paste(colnames(series_novomodelo)[1], "~", 
#                                paste(colnames(series_novomodelo)[2:(ncol(series_novomodelo) - 1)], collapse = " + "), 
#                                "+", colnames(series_novomodelo)[ncol(series_novomodelo)])
#     
#     cov_max = mp_series()$cov_max
#     ajustenovo_glmulti = glmulti(as.formula(formula_novomodelo), intercept = TRUE, level = 1, 
#                                  method = "h", crit = "AIC", 
#                                  plotty = FALSE, report = FALSE, fitfunction = "lm",
#                                  chunk = 1, chunks = 3, maxsize = cov_max)
#     
#     formula_final = as.formula(paste(ajustenovo_glmulti@formulas[[1]][2], ajustenovo_glmulti@formulas[[1]][1], ajustenovo_glmulti@formulas[[1]][3]))
#     formula_aux = paste(formula_final[[2]], formula_final[[1]], 1)
#     # formula_final
#     
#     aux_formula = strsplit(as.character(formula_final[3]), " ")
#     aux_formula = aux_formula[[1]][! aux_formula[[1]] %in% c("1", "+")]
#     
#     teste_def = regexpr('def2', aux_formula)
#     for(j in 1:length(aux_formula)){
#       if(teste_def[j] < 1){
#         formula_aux = paste(formula_aux, "+", aux_formula[j])
#       }else{
#         formula_aux = paste(formula_aux, "+", 
#                             paste0(substr(as.character(aux_formula[j]), 1, nchar(as.character(aux_formula[j]))-4), 
#                                    "def1"), 
#                             "+", aux_formula[j])
#       }
#     }
#     
#     modelo_final = lm(as.formula(formula_aux))
#     aux_col = strsplit(formula_aux, split = " ") 
#     
#     # Saídas da função
#     ano = year(as.Date(series_novomodelo)[1])
#     mes = month(as.Date(series_novomodelo)[1])
#     serie_y = series_novomodelo[,1]
#     serie_reg = ts(modelo_final$fitted.values, start = c(ano, mes), frequency=12)
#     
#     series_output = data.frame(series_novomodelo[,colnames(series_novomodelo) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]],
#                                mp_series()$series_def1[,colnames(mp_series()$series_def1) %in% aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]])
#     series = ts(series_output, start = c(ano, mes), frequency = 12)
#     series_modelo = series[,aux_col[[1]][!aux_col[[1]] %in% c("~", "1", "+")]]
#     
#     output = list(modelo = formula_aux, 
#                   resumo_modelo = summary(modelo_final),
#                   residuos = modelo_final$residuals,
#                   # serie_15maisnovo = series_novomodelo,
#                   serie_ajustada = serie_reg,
#                   serie_reg = serie_reg,
#                   series_modelo = series_modelo,
#                   lm = modelo_final)
#     output
#   })
#   
#   output$progress_bar <- renderPrint({
#     
#     #       if(input$atualizar2){
#     #        withProgress(message = 'Carregando', value = 0, {
#     #        for (i in 1:10) {
#     #          # Increment the progress bar, and update the detail text.
#     #          incProgress(0.1)
#     #                    
#     #          # Pause for 0.1 seconds to simulate a long computation.
#     #          Sys.sleep(4)
#     #        }
#     #        
#     #        })}
#     
#     if(input$atualizar2 & !is.null(mp_series())){
#       withProgress(message = 'Carregando', value = 0, {
#         for (i in 1:100) {
#           # Increment the progress bar, and update the detail text.
#           incProgress(0.01)
#           
#           # Pause for 0.1 seconds to simulate a long computation.
#           Sys.sleep(0.05)
#         }
#         
#       })}
#     
#     
#   })
#   
#   
#   # Saídas do modelo
#   
#   # Tempo
#   output$tempo_mp <- renderPrint({
#     
#     mp_series()$tempo
#     
#   })
#   
#   # Gráficos - Análise Gráfica
#   output$mp_graficoseries <- renderDygraph({
#     
#     if(value_atualizar$k == 0){
#       
#       dados = ts(data.frame(data = c(1,5,2,9,8,7,2,3,7,5,4,1), data2 = NA), start = c(2012,01), freq = 12)
#       
#       dygraph(dados)%>%
#         dySeries("data", color = "white") %>%
#         # dyHighlight(highlightCircleSize = 3, 
#         #             highlightSeriesBackgroundAlpha = 0.2,
#         #             hideOnMouseOut = FALSE) %>%
#         # dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
#         dyAxis("x", drawGrid = FALSE, axisLineColor = "white", axisLabelColor = "white") %>%
#         dyAxis("y", drawGrid = FALSE, axisLineColor = "white", axisLabelColor = "white") %>%
#         dyLegend(width = 0)
#       
#     }else{
#       
#       if(ntext()$var == "Sim"){variacao_aux = TRUE}else{variacao_aux = FALSE}
#       if(is.null(ntext()$variaveis)){
#         # Graficos
#         if(variacao_aux == TRUE){
#           
#           aux_p1 = as.ts(cbind(Custo_Observado = mp_series()$series_modelo[,1], 
#                                Custo_Ajustado = mp_series()$serie_ajustada))
#           data_aux = as.Date(aux_p1)
#           
#           dygraph(aux_p1)%>%
#             dySeries("Custo_Observado", strokeWidth = 1.5, strokePattern = "solid", color = "steelblue", label = "Custo Observado") %>%
#             dySeries("Custo_Ajustado", strokeWidth = 1.5, strokePattern = "dotted", color = "#707070", label = "Custo Ajustado") %>%
#             dyAxis("x", drawGrid = FALSE) %>%
#             dyAxis("y", label = "Custo em R$") %>%
#             dyOptions(gridLineColor = "lightgray") %>%
#             dyLegend(labelsSeparateLines = TRUE) %>%
#             dyRangeSelector(fillColor = "#A8A8A8",
#                             dateWindow = c(data_aux[1], data_aux[length(data_aux)]),
#                             strokeColor = "")
#           
#         }else{
#           
#           inicio = nrow(mp_series()$series) - length(mp_series()$serie_ajustada)
#           
#           aux1 = mp_series()$series[inicio:(nrow(mp_series()$series) - 1), 2]
#           aux2 = ((mp_series()$serie_ajustada/100) + 1)*aux1
#           
#           ano = as.integer(tsp(mp_series()$series_modelo)[1])
#           mes = as.integer((tsp(mp_series()$series_modelo)[1]%%1)/0.083) + 1
#           
#           serie_ajustada = ts(aux2, start = c(ano, mes), freq = 12)
#           serie_original = ts(mp_series()$series[(inicio + 1):nrow(mp_series()$series), 2],
#                               start = c(ano, mes), freq = 12)
#           
#           data_aux = as.Date(serie_original)
#           
#           aux_p1 = as.ts(cbind(Custo_Observado = serie_original,
#                                Custo_Ajustado = serie_ajustada))
#           dygraph(aux_p1)%>%
#             dySeries("Custo_Observado", strokeWidth = 1.5, strokePattern = "solid", color = "steelblue", label = "Custo Observado") %>%
#             dySeries("Custo_Ajustado", strokeWidth = 1.5, strokePattern = "dotted", color = "#707070", label = "Custo Ajustado") %>%
#             dyAxis("x", drawGrid = FALSE) %>%
#             dyAxis("y", label = "Custo em R$") %>%
#             dyOptions(gridLineColor = "lightgray") %>%
#             dyLegend(labelsSeparateLines = TRUE) %>%
#             dyRangeSelector(fillColor = "#A8A8A8",
#                             dateWindow = c(data_aux[1], data_aux[length(data_aux)]),
#                             strokeColor = "")
#         }}else{
#           if(variacao_aux == TRUE){
#             
#             aux_p1 = as.ts(cbind(Preco_Transf = novomodelo_reactive_series()$series_modelo[,1],
#                                  Preco_Ajustado = novomodelo_reactive_series()$serie_ajustada))
#             data_aux = as.Date(aux_p1)
#             
#             dygraph(aux_p1)%>%
#               dySeries("Preco_Transf", strokeWidth = 1.5, strokePattern = "solid", color = "steelblue", label = "Custo Observado") %>%
#               dySeries("Preco_Ajustado", strokeWidth = 1.5, strokePattern = "dotted", color = "#707070", label = "Custo Ajustado") %>%
#               
#               dyAxis("x", drawGrid = FALSE) %>%
#               dyAxis("y", label = "Custo em R$") %>%
#               dyOptions(gridLineColor = "lightgray") %>%
#               dyLegend(labelsSeparateLines = TRUE) %>%
#               dyRangeSelector(fillColor = "#A8A8A8",
#                               dateWindow = c(data_aux[1], data_aux[length(data_aux)]),
#                               strokeColor = "")
#             
#           }else{
#             
#             inicio = nrow(mp_series()$series) - length(novomodelo_reactive_series()$serie_ajustada)
#             
#             aux1 = mp_series()$series[inicio:(nrow(mp_series()$series) - 1), 2]
#             aux2 = ((novomodelo_reactive_series()$serie_ajustada/100) + 1)*aux1
#             
#             ano = as.integer(tsp(mp_series()$series_modelo)[1])
#             mes = as.integer((tsp(mp_series()$series_modelo)[1]%%1)/0.083) + 1
#             
#             serie_ajustada = ts(aux2, start = c(ano, mes), freq = 12)
#             serie_original = ts(mp_series()$series[(inicio + 1):nrow(mp_series()$series), 2],
#                                 start = c(ano, mes), freq = 12)
#             
#             data_aux = as.Date(serie_original)
#             
#             aux_p1 = as.ts(cbind(Preco_Transf = serie_original,
#                                  Preco_Ajustado = serie_ajustada))
#             dygraph(aux_p1)%>%
#               dySeries("Preco_Transf", strokeWidth = 1.5, strokePattern = "solid", color = "steelblue", label = "Custo do Transformador") %>%
#               dySeries("Preco_Ajustado", strokeWidth = 1.5, strokePattern = "dotted", color = "#707070", label = "Custo Ajustado") %>%
#               
#               dyAxis("x", drawGrid = FALSE) %>%
#               dyAxis("y", label = "Custo em R$") %>%
#               dyOptions(gridLineColor = "lightgray") %>%
#               dyLegend(labelsSeparateLines = TRUE) %>%
#               dyRangeSelector(fillColor = "#A8A8A8",
#                               dateWindow = c(data_aux[1], data_aux[length(data_aux)]),
#                               strokeColor = "")
#           }
#         }
#     }
#   })
#   
#   # Resumo do modelo
#   output$summary_mpseries <- renderPrint({
#     
#     if(value_atualizar$k == 0){a = " "; a}else{
#       if(is.null(ntext()$variaveis)){
#         mp_series()$resumo_modelo
#       }else{novomodelo_reactive_series()$resumo_modelo}
#     }
#   }) 
#   
#   
#   # Gráfico - Análise de Resíduos
#   output$mp_grafico_res_series <- renderDygraph({
#     
#     if(value_atualizar$k == 0){
#       
#       dados = ts(data.frame(data = c(1,5,2,9,8,7,2,3,7,5,4,1), data2 = NA), start = c(2012,01), freq = 12)
#       
#       dygraph(dados)%>%
#         dySeries("data", color = "white") %>%
#         dyAxis("x", drawGrid = FALSE, axisLineColor = "white", axisLabelColor = "white") %>%
#         dyAxis("y", drawGrid = FALSE, axisLineColor = "white", axisLabelColor = "white") %>%
#         dyLegend(width = 0)
#       
#     }else{
#       
#       if(is.null(ntext()$variaveis)){
#         outro_dado <- mp_series()$serie_ajustada
#         
#         # encontrar as datas iniciais e finais da série temporal
#         mes_final <- end(outro_dado)[2]
#         ano_final <- end(outro_dado)[1]
#         mes_inicial <- start(outro_dado)[2]
#         ano_inicial <- start(outro_dado)[1]
#         
#         residuos <- ts(mp_series()$residuos, start = c(ano_inicial,mes_inicial), freq = 12)
#         
#         # Fazer o gráfico
#         dygraph(residuos, main = " ") %>%
#           dyOptions(colors = "#555555") %>% 
#           dyRangeSelector(fillColor = "#A8A8A8",
#                           dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                          paste0(ano_final,"-",mes_final,"-01")),
#                           strokeColor = "")
#       }else{
#         
#         outro_dado <- novomodelo_reactive_series()$serie_ajustada
#         
#         # encontrar as datas iniciais e finais da série temporal
#         mes_final <- end(outro_dado)[2]
#         ano_final <- end(outro_dado)[1]
#         mes_inicial <- start(outro_dado)[2]
#         ano_inicial <- start(outro_dado)[1]
#         
#         residuos <- ts(novomodelo_reactive_series()$residuos, start = c(ano_inicial,mes_inicial), freq = 12)
#         
#         # Fazer o gráfico
#         dygraph(residuos, main = " ") %>%
#           dyOptions(colors = "#555555") %>% 
#           dyRangeSelector(fillColor = "#A8A8A8",
#                           dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                          paste0(ano_final,"-",mes_final,"-01")),
#                           strokeColor = "")
#         
#       }
#     }
#     
#   })
#   
#   
#   # Excluindo séries do modelo - Seleção de Covariáveis
#   
#   # Encontrando nome das covariáveis escolhidas pelo modelo
#   nomes_series <- reactive({
#     
#     fgv_aux$Descricao = as.character(fgv_aux$Descricao)
#     fgv_aux$Descricao_aux = as.character(fgv_aux$Descricao_aux)
#     fgv_aux$Nome_Bonito = as.character(fgv_aux$Nome_Bonito)
#     fgv_aux$Descricao_Completa = as.character(fgv_aux$Descricao_Completa)
#     fgv_aux$Descricao_def1 = paste0(fgv_aux$Descricao_Completa, "_def1")
#     fgv_aux$Descricao_def2 = paste0(fgv_aux$Descricao_Completa, "_def2")
#     
#     if(is.null(ntext()$variaveis)){
#       
#       nomes_coef = names(mp_series()$lm$coef)[-1]
#       nomes_codigos = names(mp_series()$lm$coef)[-1]
#       
#     }else{ 
#       nomes_coef = names(novomodelo_reactive_series()$lm$coef)[-1]
#       nomes_codigos = names(novomodelo_reactive_series()$lm$coef)[-1]
#     }
#     
#     nomes_codigos = sub(nomes_codigos, pattern = "_def1", replacement = "")
#     nomes_codigos = sub(nomes_codigos, pattern = "_def2", replacement = "")
#     nomes_codigos = unique(nomes_codigos)
#     
#     nomes_aux = fgv_aux[which(fgv_aux$Descricao_Completa %in% nomes_codigos), c("Descricao_Completa", "Nome_Bonito")]
#     
#     nomescoef_aux = nomes_coef[!nomes_coef %in% c("Preco", "Preco_def1", "Preco_def2")]
#     teste_lista = data.frame(nome_serie = nomescoef_aux)
#     teste_lista$Teste = NA
#     for(i in 1:nrow(teste_lista)){
#       
#       teste_lista[i, "Teste"] = fgv_aux[which(fgv_aux$Descricao_Completa == as.character(teste_lista$nome_serie[i]) |
#                                                 fgv_aux$Descricao_def1 == as.character(teste_lista$nome_serie[i]) |
#                                                 fgv_aux$Descricao_def2 == as.character(teste_lista$nome_serie[i])), "Nome_Bonito"]
#     }
#     
#     
#     teste3_lista = list(NA)
#     nomes_unicos = unique(teste_lista$Teste)
#     for(k in 1:length(nomes_unicos)){
#       
#       objetos = paste(as.character(teste_lista[which(teste_lista$Teste == nomes_unicos[k]), "nome_serie"]),
#                       collapse = "/")
#       teste3_lista[[k + 1]] = objetos
#       
#     }
#     teste3_lista = teste3_lista[-1]
#     names(teste3_lista) = nomes_unicos
#     teste3_lista
#     
#   })
#   
#   # 
#   output$aux_correcao = renderPrint({
#     
#     list(nomes_series(),
#          dim(dados_mp3()))
#     
#   })
#   
#   
#   # Teste
#   output$nomes_novomodelo <- renderPrint({
#     
#     if(is.null(ntext()$variaveis)){
#       
#       lista = list(coef = names(mp_series()$lm$coef))
#       
#     }else{
#       
#       lista = list(ntext = ntext()$variaveis,
#                    coef_novo = names(novomodelo_reactive_series()$lm$coef))
#       
#     }
#     lista
#     
#     
#   }) 
#   
#   # Séries excluídas
#   series_excluidas <- reactive({
#     
#     series = as.character(nomes_series()[which(names(nomes_series()) %in% input$ipc_itens)])
#     series
#     
#   })
#   
#   # Checkbox para exclusão de séries
#   output$choose_columns_series <- renderUI({
#     
#    
#     # nomes = as.character(unique(names(nomes_series())))
#     nomes = nomes_series()
#     checkboxGroupInput("ipc_itens", label = "Selecione as variáveis a serem excluídas:", width = "100%", choices = nomes)
#     
#   })
#   
#   #
#   checkbox_series <- reactive({
#     
#     str_split(input$ipc_itens, "/")
#     
#   })
#   
#   #
#   output$checkbox_series2 <- renderPrint({
#     
#     unlist(checkbox_series())
#     
#   })
#   
#   
#   # 
#   output$outro_teste <- renderPrint({
#     
#     nomes_series()
#     
#   })
#   
#   
#   # Teste subset
#   output$series_novomodelo <- renderPrint({
#     
#     if(is.null(ntext()$variaveis)){
#       summary(mp_series()$lm)}else{
#         summary(novomodelo_reactive_series()$lm)}
#     
#   })
#   
#   # 
#   output$plot <- renderPlot({
#     input$atualizar2 # Re-run when button is clicked
#     
#     withProgress(message = 'Creating plot', value = 0.1, {
#       Sys.sleep(0.25)
#       
#       # Create 0-row data frame which will be used to store data
#       dat <- data.frame(x = numeric(0), y = numeric(0))
#       
#       # withProgress calls can be nested, in which case the nested text appears
#       # below, and a second bar is shown.
#       withProgress(message = 'Generating data', detail = "part 0", value = 0, {
#         for (i in 1:10) {
#           # Each time through the loop, add another row of data. This a stand-in
#           # for a long-running computation.
#           dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
#           
#           # Increment the progress bar, and update the detail text.
#           incProgress(0.1, detail = paste("part", i))
#           
#           # Pause for 0.1 seconds to simulate a long computation.
#           Sys.sleep(0.1)
#         }
#       })
#       
#       # Increment the top-level progress indicator
#       incProgress(0.5)
#       
#       # Another nested progress indicator.
#       # When value=NULL, progress text is displayed, but not a progress bar.
#       withProgress(message = 'And this also', detail = "This other thing",
#                    value = NULL, {
#                      
#                      Sys.sleep(0.75)
#                    })
#       
#       # We could also increment the progress indicator like so:
#       # incProgress(0.5)
#       # but it's also possible to set the progress bar value directly to a
#       # specific value:
#       setProgress(1)
#     })
#   })
#   
#   # Contadores
#   value_reiniciar <- reactiveValues(i = 0)
#   value_atualizar <- reactiveValues(k = 0)
#   
#   # Contador atualizar
#   observe({
#     
#     input$atualizar2
#     
#     # We need to use isolate because otherwise whenever values$i changes
#     # the observer will be run again and we'll get stuck in an infinite 
#     # loop
#     isolate({
#       value_atualizar$k <- value_atualizar$k + 1
#     })
#   })
#   
#   # Action Button (atualizar2)
#   output$botao_reiniciar <- renderUI({
#     
#     if(input$atualizar2 & !is.null(mp_series())){
#       withProgress(message = 'Carregando', value = 0, {
#         for (i in 1:100) {
#           # Increment the progress bar, and update the detail text.
#           incProgress(0.01)
#           
#           # Pause for 0.1 seconds to simulate a long computation.
#           Sys.sleep(0.05)
#         }
#         
#       })}
#     
#     # actionButton("reiniciar","Reiniciar", width = '145px')
#     hr()
#     
#   })
#   
#   # Condição para os gráficos
#   output$condicao <- reactive({
#     
#     if(value_atualizar$k == 0){condicao = 0}else{condicao = 1}
#     condicao
#     
#   })
#   outputOptions(output, 'condicao', suspendWhenHidden=FALSE)
#   
#   # Atualizar entradas
#   observe({
#     input$reiniciar
#     updateTextInput(session, "cov_max", value = 0)
#     updateRadioButtons(session, "variacao", choices = c("Sim", "Não"), inline = TRUE, selected = "Sim")
#     updateCheckboxInput(session, "incc", value = 0)
#     updateCheckboxInput(session, "ipa", value = 0)
#     updateCheckboxInput(session, "ipc", value = 0)
#     updateCheckboxInput(session, "todas_as_series", value = 0)
#     teste <<- NA
#     isolate({value_atualizar$k <- 0})
#   })
#   
#   
#   
#   
#   
#   ### Previsão ARIMA -----------------------------------------------------
#   
#   # Executar a função do modelo ARIMA
#   ajuste_arima <- reactive({
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     ordens <- auto_arima(importado_arima()$ts)
#     modelo <- Arima(importado_arima()$ts, order = c(ordens[1],ordens[6],ordens[2]),
#                     seasonal = list(order = c(ordens[3],ordens[7],ordens[4]), period = 12))
#     list(modelo = modelo, ordens = ordens)
#     
#   })
#   
#   
#   # teste t modelo ARIMA
#   output$t_test_arima <- renderPrint({
#     t_test(importado_arima()$ts, ajuste_arima()$modelo)
#   }, width = 100)
#   
#   # modelo ARIMA
#   output$texto_arima <- renderText({
#     ordens <- ajuste_arima()$ordens
#     paste0("ARIMA(",ordens[1]," ",ordens[6]," ",ordens[2],")(",ordens[3]," ",ordens[7]," ",ordens[4],")[12]")
#   })
#   
#   # gráfico do ajuste ARIMA
#   output$arima_grafico <- renderDygraph({
#     
#     y <- importado_arima()$ts
#     ychapeu <- fitted(ajuste_arima()$modelo)
#     dados <- cbind(y,ychapeu)
#     colnames(dados) <- c("Observados","Ajustados")
#     
#     # encontrar as datas iniciais e finais da série temporal
#     mes_final <- end(dados)[2]
#     ano_final <- end(dados)[1]
#     mes_inicial <- start(dados)[2]
#     ano_inicial <- start(dados)[1]
#     
#     # Fazer o gráfico
#     dygraph(dados[,1:2], main = "Observados vs. Ajustados", ) %>%
#       dySeries("Observados", strokeWidth = 1, color = "black") %>%
#       dySeries("Ajustados", strokeWidth = 2, color = "dodgerblue") %>%
#       dyRangeSelector(fillColor = "skyblue",
#                       dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                      paste0(ano_final,"-",mes_final,"-01")))
#   })
#   
#   # gráfico dos resíduos do modelo ARIMA
#   output$arima_grafico_res <- renderDygraph({
#     
#     res <- resid(ajuste_arima()$modelo)
#     residuos <- (res - mean(res))/sd(res) #resíduos padronizados
#     
#     # encontrar as datas iniciais e finais da série temporal
#     mes_final <- end(residuos)[2]
#     ano_final <- end(residuos)[1]
#     mes_inicial <- start(residuos)[2]
#     ano_inicial <- start(residuos)[1]
#     
#     # Fazer o gráfico
#     dygraph(residuos, main = "Gráfico dos resíduos padronizados") %>%
#       dySeries("V1", label = "Resíduo") %>%
#       dyOptions(colors = "black") %>%
#       dyRangeSelector(fillColor = "skyblue",
#                       dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                      paste0(ano_final,"-",mes_final,"-01")))
#   })
#   
#   
#   # previsão do modelo ARIMA
#   prev_arima <- reactive({
#     p <- forecast(ajuste_arima()$modelo, h = 12, level = 0.95)
#     p2 <- data.frame(Estimativa = c(p$mean), inf = c(p$lower), sup = c(p$upper))
#     prev <- ts(p2, start = c(start(p$mean)[1],start(p$mean)[2]), freq = 12)
#   })
#   
#   # Gráfico da previsão modelo ARIMA
#   output$arima_previsao <- renderDygraph({
#     dygraph(prev_arima(), main = "Previsão 12 passos à frente" ) %>%
#       dySeries(c("inf" ,"Estimativa","sup"), color = "dodgerblue", strokeWidth = 1, drawPoints = TRUE, pointSize = 3,strokePattern = "dashed")
#   })
#   
#   
#   # Tabela da previsão modelo ARIMA
#   tabela_arima <- reactive({
#     data <- substr(as.Date(prev_arima()),1,7)
#     tabela <- matrix(NA, ncol = 12, nrow = 3)
#     colnames(tabela) <- data
#     rownames(tabela) <- c("Lim. Inf", "Estimativa", "Lim. Sup")
#     dados <- as.data.frame(prev_arima())
#     tabela[1,] <- dados$inf
#     tabela[2,] <- dados$Estimativa
#     tabela[3,] <- dados$sup
#     tabela
#   })
#   
#   output$tabela_previsao <- renderTable({
#     tabela_arima()
#   })
#   
#   # Tabela de resultados do modelo ARIMA para exportação
#   exportar_arima <- reactive({
#     
#     dados <- importado_arima()$ts
#     
#     tabela1 <- matrix(NA, nrow = length(dados) + 12, ncol = 5)
#     tabela1[,2] <- c(importado_arima()$ts,rep(NA,12))
#     tabela1[,3] <- c(fitted(ajuste_arima()$modelo),rep(NA,12))
#     tabela1[,4] <- c(resid(ajuste_arima()$modelo),rep(NA,12))
#     tabela1[(length(dados)+1):(length(dados)+12),5] <- c(tabela_arima()[2,])
#     
#     tabela1 <- data.frame(tabela1)
#     tabela1[,1] <- as.character(seq(as.Date(paste0(start(dados)[1],"/",start(dados)[2],"/",1)),
#                                     as.Date(paste0(end(dados)[1]+1,"/",end(dados)[2],"/",1)),
#                                     "1 months"))
#     
#     colnames(tabela1) <- c("Data", "Observados", "Ajustados","Resíduos","Previsão")
#     tabela1
#     
#   })
#   
#   # ------------------------------------------------------------------------------------------------------
#   # BANCO DE SÉRIES TEMPORAIS  ---------------------------------------------------------------------------
#   # ------------------------------------------------------------------------------------------------------
#   
#   # página inicial - busca -----------------------------------------
#   # filtrando
#   tabela_busca <- reactive({
#     a_busca <- switch(input$Busca,"Codigo" = "codigo","Palavras"= "Palavras")
#     
#     if(a_busca=="codigo"){
#       escolher_codigos <- as.numeric(input$searchText)
#       subset(infos[,c("Código", "Nome", "Unidade","Periodicidade","Início","Fim", "Base")],infos[,1]==escolher_codigos)
#     }else if(a_busca=="Palavras"){
#       escolher_palavras <- casefold(strsplit(input$searchText, " ")[[1]])
#       masc <- grepl(escolher_palavras [1],casefold(infos[,2]))    
#       if(length(escolher_palavras) > 1){
#         for(i in 2:length(escolher_palavras)){
#           masc <- masc & grepl(escolher_palavras [i],casefold(infos[,2]))
#         }
#       }
#       infos$teste <- masc
#       subset(infos[,c("Código", "Nome","Periodicidade","Início","Fim")],infos$teste==TRUE)            
#     }
#     
#   })
#   
#   # retornar resultados da busca
#   output$tabela_completa <- DT::renderDataTable({      
#     input$buscat
#     if(input$buscat == 0){ return("")
#     }else{
#       isolate({
#         withProgress(message = 'Carregando:', value = 0, {
#           n <- 100
#           for (i in 1:n) {
#             incProgress(1/n, detail = paste0(i,"%"))
#             # Pause for 0.02 seconds to simulate a long computation.
#             Sys.sleep(0.01)
#           }
#         })
#         
#         # retornar tabela
#      datatable(tabela_busca(), 
#                   rownames = tabela_busca()[,"Código"], #checkboxRows(tabela_busca()),
#                   escape = -1, class = 'row-border stripe compact hover',
#                   options = list(lengthMenu = c(5, 10, 20),
#                                  pageLength = 10,dom="tip"))
# 
#         
#       })
#     }
#   })
#   
#   # código/linhas/séries selecionadas na tabela busca
#   codigos_busca <- reactive({
#     # linhas selecionadas
#      codigos <- input$tabela_completa_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#      codigos <- gsub(" ", "", codigos, fixed = TRUE)
#      
#      codigos <-as.character(tabela_busca()[input$tabela_completa_rows_selected, "Código"])
#     # # códigos selecionados
#      tabela <- tabela_busca()[tabela_busca()[,"Código"] %in% codigos, c("Código","Nome")]
#     # # objeto com as séries temporais
#     # codigos_info <- paste0("ST_",codigos)
#     # list(dados = FGV[,codigos_info], nome = tabela[,"Nome"], codigo = codigos_info)
#     
#     #codigos<-as.numeric(input$tabela_completa_rows_selected )
#     
#     palavras<-as.character(codigos)
#     paste(palavras, collapse = "' '")
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     #tabela <- table_igp()[input$tabela_completa_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados=fgv_ts, nome = codigos_info, codigo = codigos_info)
#     
#   })
# 
#   # Mostrar legenda busca
#   output$legenda_busca <- renderTable({
#     tabela <- cbind(codigos_busca()$codigo,codigos_busca()$nome)
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#   
#   output$grafico_busca <- renderDygraph({
#     # Encontrar n
#     n <- length(codigos_busca()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(codigos_busca()$dados, main = "") %>%
#         dySeries("V1", label = codigos_busca()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(codigos_busca()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   output$dados_busca<- renderTable({
#     codigos_busca()$dados
#   })
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_busca <- renderTable({
#     
#     n <- length(codigos_busca()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(codigos_busca()$dados[,i], na.rm = T)
#         mediana[i] <- median(codigos_busca()$dados[,i], na.rm = T)
#         mini[i] <- min(codigos_busca()$dados[,i], na.rm = T)
#         maxi[i] <- max(codigos_busca()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(codigos_busca()$dados, na.rm = T)
#         mediana <- median(codigos_busca()$dados, na.rm = T)
#         mini <- min(codigos_busca()$dados, na.rm = T)
#         maxi <- max(codigos_busca()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- codigos_busca()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   ### IGP --------------------------------------------------------------------------- 
#   
#   # dataframe de informações sobre o IGP
#   table_igp <- reactive({
#     
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.2)
#       }
#     }) # fim barrinha carregando
#    
#     # dataframe só com IGP
#     subset(infos[,c("Código","Nome","Periodicidade","Início","Fim")], infos$Fonte == "Índices Gerais de Preços")
#            
#   })
#   
#   # gerar tabela do dataframe
#   output$tabela_igp <- DT::renderDataTable(
#       datatable(table_igp(), 
#                 rownames = table_igp()[,"Código"],
#                 options = list(lengthMenu = c(5, 10, 20),
#                                pageLength = 10,
#                                searching = FALSE))            
#   )
#   
#     
#   # código/linhas/séries selecionadas na tabela igp
#   dados_igp <- reactive({
#     # linhas selecionadas
#     codigos <- input$tabela_igp_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#      codigos <- gsub(" ", "", codigos, fixed = TRUE)
# #
#     codigos <-as.character(table_igp()[input$tabela_igp_rows_selected, "Código"])#_Selected: retorna as linhas selecionadas da tabela_completa
#     tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#     # códigos selecionados
#     tabela <- table_igp()[table_igp()[,"Código"] %in% codigos, c("Código","Nome")]
#     # objeto com as séries temporais
#     codigos_info <- paste0("ST_",codigos)
# 
#     #codigos<-as.numeric(table_igp[(input$tabela_igp_rows_selected),"Código"])
#     
#     palavras<-as.character(codigos)
#     paste(palavras, collapse = "' '")
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#       tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#       codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#   })
# 
#   
#   # Mostrar gráficos IGP
#   output$grafico_igp <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_igp()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_igp()$dados, main = "") %>%
#         dySeries("V1", label = dados_igp()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(dados_igp()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   # Mostrar legenda IGP
#   output$legenda_igp <- renderTable({
#     tabela <- cbind(dados_igp()$codigo,dados_igp()$nome)
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#     
#   # Mostrar dados IGP
#   output$dados_igp <- renderTable({
#     data.frame(dados_igp()$dados)
#   })
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_igp <- renderTable({
#     
#     n <- length(dados_igp()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_igp()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_igp()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_igp()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_igp()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_igp()$dados, na.rm = T)
#         mediana <- median(dados_igp()$dados, na.rm = T)
#         mini <- min(dados_igp()$dados, na.rm = T)
#         maxi <- max(dados_igp()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_igp()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   
# 
#   
#   ### -------------------------------- FIM IGP ------------------------------- #
# 
#   ### IPA --------------------------------------------------------------------
# 
#   # IPA - 10 ---------------------------------------
# #   filtro_ipa10 <- reactive({
# #     
# #     masc_10 <- substr(subset_ipa_novo()$Serviço,9,10) == substr(input$ipa_indice,5,6) #"-")[[1]][2]
# #     masc_estrutura <- subset_ipa_novo()$Estrutura == ipa_estrutura2()
# #     masc_estagio <- subset_ipa_novo()$Estagio == input$ipa10_estagios
# #     masc_grupo <- subset_ipa_novo()$Grupo == input$ipa10_grupos
# #     masc_subgrupo <- subset_ipa_novo()$Subgrupo == input$ipa10_subgrupos
# #     masc_item <- subset_ipa_novo()$Item == input$ipa10_itens
# #     
# #     if(input$ipa10_estagios == "TODOS OS ESTÁGIOS (MAIOR NÍVEL)"){
# #       masc_tudo <- subset_ipa_novo()$Cod_grupo == 0  & subset_ipa_novo()$Cod_subgrupo == 0 & subset_ipa_novo()$Cod_item == 0 & subset_ipa_novo()$Cod_subitem == 0
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_tudo,]
# #     }else if(input$ipa10_grupos == "TODOS OS GRUPOS (MAIOR NÍVEL)"){
# #       masc_tudo <- subset_ipa_novo()$Cod_subgrupo == 0 & subset_ipa_novo()$Cod_item == 0 & subset_ipa_novo()$Cod_subitem == 0
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_tudo,] 
# #     }else if(input$ipa10_estagios == "PRODUTOS AGROPECUÁRIOS"){
# #       masc_tudo <- subset_ipa_novo()$Cod_subgrupo == 0 & subset_ipa_novo()$Cod_item == 0
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,]
# #     }else if(input$ipa10_grupos %in% c("EMBALAGENS","MATERIAIS E COMPONENTES PARA A CONSTRUÇÃO", "COMBUSTÍVEIS E LUBRIFICANTES PARA A PRODUÇÃO", "MINERAIS")){
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_grupo,]
# #     }else if(input$ipa10_subgrupos == "TODOS OS SUBGRUPOS (MAIOR NÍVEL)"){
# #       masc_tudo <- subset_ipa_novo()$Cod_item == 0 & subset_ipa_novo()$Cod_subitem == 0
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,] 
# #     }else if(input$ipa10_subgrupos == "CARVÃO MINERAL"){
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo,]  
# #     }else if(input$ipa10_subgrupos %in% c("COMBUSTÍVEIS", "UTILIDADES DOMÉSTICAS", "VEÍCULOS E ACESSÓRIOS", "VESTUÁRIO, CALÇADOS E ACESSÓRIOS", "MÁQUINAS E EQUIPAMENTOS", "VEÍCULOS PESADOS", "MATERIAIS PARA A MANUFATURA", "SUPRIMENTOS NÃO AGROPECUÁRIOS", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS NÃO ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS COMERCIALIZÁVEIS", "PRODUTOS DO FUMO", "OUTROS MINERAIS NÃO-METÁLICOS")){
# #       masc_tudo <- subset_ipa_novo()$Cod_item == 0
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,]  
# #     }else if(input$ipa10_itens == "TODOS OS ITENS (MAIOR NÍVEL)"){
# #       masc_tudo <- subset_ipa_novo()$Cod_subitem == 0
# #       subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,] 
# #     }else{ subset_ipa_novo()[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_item,] }
# #   })
#   
#   filtro_ipa10 <- reactive({
#     
#     masc_10 <- substr(subset_ipa$Serviço,9,10) == strsplit(menu_selecionado(),"-")[[1]][2]
#     masc_estrutura <- subset_ipa$Estrutura == ipa_estrutura()
#     masc_estagio <- subset_ipa$Estagio == input$ipa10_estagios
#     masc_grupo <- subset_ipa$Grupo == input$ipa10_grupos
#     masc_subgrupo <- subset_ipa$Subgrupo == input$ipa10_subgrupos
#     masc_item <- subset_ipa$Item == input$ipa10_itens
#     
#     if(input$ipa10_estagios == "TODOS OS ESTÁGIOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_grupo == 0  & subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_10 & masc_estrutura & masc_tudo,]
#     }else if(input$ipa10_grupos == "TODOS OS GRUPOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_tudo,] 
#     }else if(input$ipa10_estagios == "PRODUTOS AGROPECUÁRIOS"){
#       masc_tudo <- subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0
#       subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,]
#     }else if(input$ipa10_grupos %in% c("EMBALAGENS","MATERIAIS E COMPONENTES PARA A CONSTRUÇÃO", "COMBUSTÍVEIS E LUBRIFICANTES PARA A PRODUÇÃO", "MINERAIS")){
#       subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_grupo,]
#     }else if(input$ipa10_subgrupos == "TODOS OS SUBGRUPOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,] 
#     }else if(input$ipa10_subgrupos == "CARVÃO MINERAL"){
#       subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo,]  
#     }else if(input$ipa10_subgrupos %in% c("COMBUSTÍVEIS", "UTILIDADES DOMÉSTICAS", "VEÍCULOS E ACESSÓRIOS", "VESTUÁRIO, CALÇADOS E ACESSÓRIOS", "MÁQUINAS E EQUIPAMENTOS", "VEÍCULOS PESADOS", "MATERIAIS PARA A MANUFATURA", "SUPRIMENTOS NÃO AGROPECUÁRIOS", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS NÃO ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS COMERCIALIZÁVEIS", "PRODUTOS DO FUMO", "OUTROS MINERAIS NÃO-METÁLICOS")){
#       masc_tudo <- subset_ipa$Cod_item == 0
#       subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,]  
#     }else if(input$ipa10_itens == "TODOS OS ITENS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,] 
#     }else{ subset_ipa[masc_10 & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_item,] }
#   })
#   
#   
#   # Gerar tabela filtrada
#   output$filtro_ipa10 <- renderDataTable({
#     if(input$visu_ipa10 == 0){ return("")
#     }else{
#       isolate({
#         
#         # barrinha carregando
#         withProgress(message = 'Carregando:', value = 0, {
#           n <- 100
#           for (i in 1:n) {
#             incProgress(1/n, detail = paste0(i,"%"))
#             Sys.sleep(0.001) 
#           }
#         }) # fim barrinha carregando
#         # gerar tabela
#         datatable(filtro_ipa10()[,c("Código","Nome","Periodicidade","Início","Fim")], 
#                   rownames = filtro_ipa10()[,"Código"],
#                   options = list(lengthMenu = c(5, 10, 20),
#                                  pageLength = 10))
#       })}
#   })
# 
#   # BOTÃO VISUALIZAR SÉRIES IPA-10 --------------------------------------- #
#   # código/linhas/séries selecionadas na tabela ipa10
#   dados_ipa10 <- reactive({
#     # #linhas selecionadas
#     # codigos <- input$filtro_ipa10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     # codigos <-as.character(filtro_ipa10()[input$tabela_igp_rows_selected, "Código"])
#     # codigos_fgv <- paste0("ST_",codigos)
#     # tabela <- filtro_ipa10()[filtro_ipa10()[,"Código"] %in% codigos, c("Código","Nome")]
#     # list(dados = FGV[,codigos_fgv], nome = tabela[,"Nome"], codigo = codigos_fgv)
#     # #codigos<-as.numeric(input$filtro_ipa10_rows_selected)
#     # 
#     # palavras<-as.character(codigos)
#     
#     
#     
#     codigos <- input$filtro_ipa10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     #
#     codigos <-as.character(filtro_ipa10()[input$filtro_ipa10_rows_selected, "Código"])#_Selected: retorna as linhas selecionadas da tabela_completa
#     tabela <- filtro_ipa10()[input$filtro_ipa10_rows_selected, c("Código","Nome")]
#     # códigos selecionados
#     tabela <- filtro_ipa10()[filtro_ipa10()[,"Código"] %in% codigos, c("Código","Nome")]
#     # objeto com as séries temporais
#     codigos_info <- paste0("ST_",codigos)
#     
#     #codigos<-as.numeric(table_igp[(input$tabela_igp_rows_selected),"Código"])
#     
#     palavras<-as.character(codigos)
#     paste(palavras, collapse = "' '")
#     
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#   
#     
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela <- table_igp()[input$filtro_ipa10_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#     
#   })
# 
#   # Mostrar gráficos IPA-10
#   output$grafico_ipa10 <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_ipa10()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_ipa10()$dados, main = "") %>%
#         dySeries("V1", label = dados_ipa10()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector(fillColor = "#2C65A1")
#     }else{
#       dygraph(dados_ipa10()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#  
#     # Mostrar legenda IPA-10
#     output$legenda_ipa10 <- renderTable({
#       tabela <- cbind(dados_ipa10()$codigo,as.character(dados_ipa10()$Nome))
#       colnames(tabela) <- c("Código", "Nome")
#       tabela
#     })
#     
#     # Mostrar dados IPA-10
#     output$dados_ipa10 <- renderTable({
#       dados_ipa10()$dados
#     })
#    
#     # Mostrar estatísticas descritivas IPA-10
#     output$descritiva_ipa10 <- renderTable({
#       
#       n <- length(dados_ipa10()$nome)
#       media <- 0
#       mediana <- 0
#       mini <- 0 
#       maxi <- 0
#       
#       if(n > 1){
#         for(i in 1:n){
#           media[i] <- mean(dados_ipa10()$dados[,i], na.rm = T)
#           mediana[i] <- median(dados_ipa10()$dados[,i], na.rm = T)
#           mini[i] <- min(dados_ipa10()$dados[,i], na.rm = T)
#           maxi[i] <- max(dados_ipa10()$dados[,i], na.rm = T)
#         }}else{
#           media <- mean(dados_ipa10()$dados, na.rm = T)
#           mediana <- median(dados_ipa10()$dados, na.rm = T)
#           mini <- min(dados_ipa10()$dados, na.rm = T)
#           maxi <- max(dados_ipa10()$dados, na.rm = T)
#         }
#       
#       tabela <- matrix(NA, ncol = 4, nrow = n)
#       colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#       rownames(tabela) <- dados_ipa10()$nome
#       tabela[,1] <- media
#       tabela[,2] <- mediana
#       tabela[,3] <- mini
#       tabela[,4] <- maxi
#       tabela
#     })
#     # FIM BOTÃO VISUALIZAR SÉRIES IPA-10 --------------------------------------- # 
# 
#   # IPA - DI ---------------------------------------------
#   filtro_ipadi <- reactive({
#     
#     masc_di <- substr(subset_ipa$Serviço,9,10) == strsplit(menu_selecionado(), "-")[[1]][2]
#     masc_estrutura <- subset_ipa$Estrutura == ipa_estrutura()
#     masc_estagio <- subset_ipa$Estagio == input$ipadi_estagios
#     masc_grupo <- subset_ipa$Grupo == input$ipadi_grupos
#     masc_subgrupo <- subset_ipa$Subgrupo == input$ipadi_subgrupos
#     masc_item <- subset_ipa$Item == input$ipadi_itens
#     
#     if(input$ipadi_estagios == "TODOS OS ESTÁGIOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_grupo == 0  & subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_di & masc_estrutura & masc_tudo,]
#     }else if(input$ipadi_grupos == "TODOS OS GRUPOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_tudo,] 
#     }else if(input$ipadi_estagios == "PRODUTOS AGROPECUÁRIOS"){
#       masc_tudo <- subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0
#       subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,]
#     }else if(input$ipadi_grupos %in% c("EMBALAGENS","MATERIAIS E COMPONENTES PARA A CONSTRUÇÃO", "COMBUSTÍVEIS E LUBRIFICANTES PARA A PRODUÇÃO", "MINERAIS")){
#       subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_grupo,]
#     }else if(input$ipadi_subgrupos == "TODOS OS SUBGRUPOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,] 
#     }else if(input$ipadi_subgrupos == "CARVÃO MINERAL"){
#       subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo,]  
#     }else if(input$ipadi_subgrupos %in% c("COMBUSTÍVEIS", "UTILIDADES DOMÉSTICAS", "VEÍCULOS E ACESSÓRIOS", "VESTUÁRIO, CALÇADOS E ACESSÓRIOS", "MÁQUINAS E EQUIPAMENTOS", "VEÍCULOS PESADOS", "MATERIAIS PARA A MANUFATURA", "SUPRIMENTOS NÃO AGROPECUÁRIOS", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS NÃO ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS COMERCIALIZÁVEIS", "PRODUTOS DO FUMO", "OUTROS MINERAIS NÃO-METÁLICOS")){
#       masc_tudo <- subset_ipa$Cod_item == 0
#       subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,]  
#     }else if(input$ipadi_itens == "TODOS OS ITENS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,] 
#     }else{ subset_ipa[masc_di & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_item,] }
#   })
#     
#     
#     
#   
#   # GERAR TABELA IPA-DI
#   output$filtro_ipadi <- renderDataTable({
#     if(input$visu_ipadi == 0){ return("")
#     }else{
#       isolate({
#         
#         # barrinha carregando
#         withProgress(message = 'Carregando:', value = 0, {
#           n <- 100
#           for (i in 1:n) {
#             incProgress(1/n, detail = paste0(i,"%"))
#             Sys.sleep(0.001) 
#           }
#         }) # fim barrinha carregando
#         # gerar tabela
#         datatable(filtro_ipadi()[,c("Código","Nome","Periodicidade","Início","Fim")], 
#                   rownames = filtro_ipadi()[,"Código"],
#                   options = list(lengthMenu = c(5, 10, 20),
#                                  pageLength = 10))
#       })}
#   })
#   
#   # BOTÃO VISUALIZAR SÉRIES IPA-DI --------------------------------------- #
#   # código/linhas/séries selecionadas na tabela ipadi
#   dados_ipadi <- reactive({
#     #linhas selecionadas
#     codigos <- input$filtro_ipadi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     codigos <-as.character(filtro_ipadi()[input$filtro_ipadi_rows_selected, "Código"])
#     codigos_fgv <- paste0("ST_",codigos)
#     tabela <- filtro_ipadi()[filtro_ipadi()[,"Código"] %in% codigos, c("Código","Nome")]
#     list(dados = FGV[,codigos_fgv], nome = tabela[,"Nome"], codigo = codigos_fgv)
#     #codigos<-as.numeric(input$filtro_ipadi_rows_selected)
#     
#     
#     palavras<-as.character(codigos)
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela <- table_igp()[input$filtro_ipadi_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#   })
# 
#   # Mostrar gráficos IPA-DI
#   output$grafico_ipadi <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_ipadi()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_ipadi()$dados, main = "") %>%
#         dySeries("V1", label = dados_ipadi()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector(fillColor = "#2C65A1")
#     }else{
#       dygraph(dados_ipadi()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   # Mostrar legenda IPA-DI
#   output$legenda_ipadi <- renderTable({
#     tabela <- cbind(dados_ipadi()$codigo,as.character(dados_ipadi()$Nome))
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#   
#   # Mostrar dados IPA-DI
#   output$dados_ipadi <- renderTable({
#     dados_ipadi()$dados
#   })
#   
#   # Mostrar estatísticas descritivas IPA-DI
#   output$descritiva_ipadi <- renderTable({
#     
#     n <- length(dados_ipadi()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_ipadi()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_ipadi()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_ipadi()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_ipadi()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_ipadi()$dados, na.rm = T)
#         mediana <- median(dados_ipadi()$dados, na.rm = T)
#         mini <- min(dados_ipadi()$dados, na.rm = T)
#         maxi <- max(dados_ipadi()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_ipadi()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   # FIM BOTÃO VISUALIZAR SÉRIES IPA-DI --------------------------------------- # 
#   
#   # IPA - M -----------------------------------
#   
#   filtro_ipam <- reactive({
#     
#     masc_m <- substr(subset_ipa$Serviço,9,10) == strsplit(menu_selecionado(), "-")[[1]][2]
#     masc_estrutura <- subset_ipa$Estrutura == ipa_estrutura()
#     masc_estagio <- subset_ipa$Estagio == input$ipam_estagios
#     masc_grupo <- subset_ipa$Grupo == input$ipam_grupos
#     masc_subgrupo <- subset_ipa$Subgrupo == input$ipam_subgrupos
#     masc_item <- subset_ipa$Item == input$ipam_itens
#     
#     if(input$ipam_estagios == "TODOS OS ESTÁGIOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_grupo == 0  & subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_m & masc_estrutura & masc_tudo,]
#     }else if(input$ipam_grupos == "TODOS OS GRUPOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_tudo,] 
#     }else if(input$ipam_estagios == "PRODUTOS AGROPECUÁRIOS"){
#       masc_tudo <- subset_ipa$Cod_subgrupo == 0 & subset_ipa$Cod_item == 0
#       subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,]
#     }else if(input$ipam_grupos %in% c("EMBALAGENS","MATERIAIS E COMPONENTES PARA A CONSTRUÇÃO", "COMBUSTÍVEIS E LUBRIFICANTES PARA A PRODUÇÃO", "MINERAIS")){
#       subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_grupo,]
#     }else if(input$ipam_subgrupos == "TODOS OS SUBGRUPOS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_item == 0 & subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_grupo & masc_tudo,] 
#     }else if(input$ipam_subgrupos == "CARVÃO MINERAL"){
#       subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo,]  
#     }else if(input$ipam_subgrupos %in% c("COMBUSTÍVEIS", "UTILIDADES DOMÉSTICAS", "VEÍCULOS E ACESSÓRIOS", "VESTUÁRIO, CALÇADOS E ACESSÓRIOS", "MÁQUINAS E EQUIPAMENTOS", "VEÍCULOS PESADOS", "MATERIAIS PARA A MANUFATURA", "SUPRIMENTOS NÃO AGROPECUÁRIOS", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS - PROCESSAMENTO INDUSTRIAL PARA FINS NÃO ALIMENTARES", "MAT. PRIMAS BRUTAS AGROPECUÁRIAS COMERCIALIZÁVEIS", "PRODUTOS DO FUMO", "OUTROS MINERAIS NÃO-METÁLICOS")){
#       masc_tudo <- subset_ipa$Cod_item == 0
#       subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,]  
#     }else if(input$ipam_itens == "TODOS OS ITENS (MAIOR NÍVEL)"){
#       masc_tudo <- subset_ipa$Cod_subitem == 0
#       subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,] 
#     }else{ subset_ipa[masc_m & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_item,] }
#   })
#   
#   # GERAR TABELA IPA-M
#   output$filtro_ipam <- renderDataTable({
#     if(input$visu_ipam == 0){ return("")
#     }else{
#       isolate({
#         
#         # barrinha carregando
#         withProgress(message = 'Carregando:', value = 0, {
#           n <- 100
#           for (i in 1:n) {
#             incProgress(1/n, detail = paste0(i,"%"))
#             Sys.sleep(0.001) 
#           }
#         }) # fim barrinha carregando
#         # gerar tabela
#         datatable(filtro_ipam()[,c("Código","Nome","Periodicidade","Início","Fim")], 
#                   rownames = filtro_ipam()[,"Código"],
#                   options = list(lengthMenu = c(5, 10, 20),
#                                  pageLength = 10))
#       })}
#   })
#   
#   # BOTÃO VISUALIZAR SÉRIES IPA-M --------------------------------------- #
#   # código/linhas/séries selecionadas na tabela ipam
#   dados_ipam <- reactive({
#     #linhas selecionadas
#     codigos <- input$filtro_ipam_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     codigos <-as.character(filtro_ipam()[input$filtro_ipam_rows_selected, "Código"])
#     codigos_fgv <- paste0("ST_",codigos)
#     tabela <- filtro_ipam()[filtro_ipam()[,"Código"] %in% codigos, c("Código","Nome")]
#     list(dados = FGV[,codigos_fgv], nome = tabela[,"Nome"], codigo = codigos_fgv)
#     
#     #codigos<-as.numeric(input$filtro_ipam_rows_selected)
#     
#     palavras<-as.character(codigos)
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela <- table_igp()[input$filtro_ipam_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#   })
# 
#   # Mostrar gráficos IPA-M
#   output$grafico_ipam <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_ipam()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_ipam()$dados, main = "") %>%
#         dySeries("V1", label = dados_ipam()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(dados_ipam()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   # Mostrar legenda IPA-M
#   output$legenda_ipam <- renderTable({
#     tabela <- cbind(dados_ipam()$codigo,as.character(dados_ipam()$Nome))
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#   
#   # Mostrar dados IPA-M
#   output$dados_ipam <- renderTable({
#     dados_ipam()$dados
#   })
#   
#   # Mostrar estatísticas descritivas IPA-M
#   output$descritiva_ipam <- renderTable({
#     
#     n <- length(dados_ipam()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_ipam()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_ipam()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_ipam()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_ipam()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_ipam()$dados, na.rm = T)
#         mediana <- median(dados_ipam()$dados, na.rm = T)
#         mini <- min(dados_ipam()$dados, na.rm = T)
#         maxi <- max(dados_ipam()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_ipam()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   # FIM BOTÃO VISUALIZAR SÉRIES IPA-M --------------------------------------- # 
#   
#   ########################IPC################################
#   
#   
#   frame_ipc<-reactiveValues()
#   
#   montando_ipc<- observe({
#     
#     if(input$visu_ipc == 0){ return("")}else{
#       #       if(input$ipc_grupos!=0 & input$ipc_subgrupos==0 & input$ipc_itens==0 & input$ipc_subitens==0){
#       #         frame_ipc$df<-rbind(subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==0)),
#       #                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==input$ipc_grupos)))}
#       #       
#       #       if(input$ipc_subgrupos!=0){
#       #         frame_ipc$df<-rbind(subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==0)),
#       #                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subgrupos,1,1))),
#       #                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subgrupos,1,2))))}
#       #       
#       #       if(input$ipc_itens!=0){
#       #         frame_ipc$df<-rbind(subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==0)),
#       #                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_itens,1,1))),
#       #                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_itens,1,2))),
#       #                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_itens,1,4))))}
#       
#       if(input$ipc_subitens!=0){
#         frame_ipc$df<-data.frame(rbind(subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==0)),
#                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_grupos,1,1))),
#                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subgrupos,1,2))),
#                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_itens,1,4))),
#                             subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subitens,1,6)))))}
#       
#     }
#     
#   })
#   
#   # gerar tabela do dataframe
#   output$tabela_ipc <-DT::renderDataTable({
#     #     datatable(table_ipc, 
#     #               rownames = table_ipc[,"Código"],
#     #               options = list(lengthMenu = c(5, 10, 20),
#     #                              pageLength = 10,
#     #                              searching = FALSE))
#     
#     # datatable(rbind(subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==0)),
#     #           subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subitens,1,1))),
#     #           subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subitens,1,2))), 
#     #           subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subitens,1,4))),
#     #           subset(subset_ipc_novo(),(subset_ipc_novo()$item_fgv==substr(input$ipc_subitens,1,6)))))
#     
#     datatable(frame_ipc$df[,c("Código","Nome","Periodicidade","Início","Fim")],
#               rownames = frame_ipc$df[,"Código"],
#               options = list(lengthMenu = c(5, 10, 20),
#                              pageLength = 10,
#                              searching = FALSE))
#   
#   })
# 
# 
# # BOTÃO VISUALIZAR SÉRIES IPC --------------------------------------- #
# # código/linhas/séries selecionadas na tabela ipc
#   
#   # código/linhas/séries selecionadas na tabela igp
# #   dados_igp <- reactive({
# #     # linhas selecionadas
# #     codigos <- input$tabela_igp_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
# #     codigos <- gsub(" ", "", codigos, fixed = TRUE)
# #     #     
# #     #codigos <-as.character(table_igp()[input$tabela_igp_rows_selected, "Código"])#_Selected: retorna as linhas selecionadas da tabela_completa
# #     tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
# #     # códigos selecionados
# #     tabela <- table_igp()[table_igp()[,"Código"] %in% codigos, c("Código","Nome")]
# #     # objeto com as séries temporais
# #     codigos_info <- paste0("ST_",codigos)
# #     list(dados = FGV[,codigos_info], nome = tabela[,"Nome"], codigo = codigos_info)
# #   })
#  
# dados_ipc <- reactive({
#   # linhas selecionadas
# #   codigos <-as.character(frame_ipc$df[input$tabela_ipc_rows_selected, "Código"])#_Selected: retorna as linhas selecionadas da tabela_completa
# #   codigos_fgv <- paste0("ST_",codigos)
# 
#   codigos <- as.character(input$tabela_ipc_rows_selected) #_Selected: retorna as linhas selecionadas da tabela_completa
#   codigos <- gsub(" ", "", codigos, fixed = TRUE)
#   codigos <-as.character(frame_ipc$df[input$tabela_ipc_rows_selected, "Código"])
#   tabela <- frame_ipc$df[input$tabela_ipc_rows_selected, c("Código","Nome")]
#   codigos_fgv <- paste0("ST_",codigos)
#   #tabela <- frame_ipc$df[input$tabela_ipc_rows_selected, c("Código","Nome")]
#   #dados = FGV[,codigos_fgv]
#   # list(dados = FGV[,codigos_fgv], nome = tabela[,"Nome"], codigo = codigos_fgv)
#   
#   
#   #codigos<-as.numeric(input$tabela_ipc_rows_selected)
#   
#   palavras<-as.character(codigos)
#   paste(palavras, collapse = "' '")
#   strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#   dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#   FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#   odbcCloseAll()
#   
#   str(FGV_base)
#   
#   ######## indo recortar a base ################################
#   base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#   start_ano=as.numeric(substr(base[1,1],1,4))
#   start_mes=as.numeric(substr(base[1,1],5,6))
#   
#   fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#   
#   if(length(codigos)>1){
#     for (i in 2:length(codigos)){
#       base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#       start_ano=as.numeric(substr(base[1,1],1,4))
#       start_mes=as.numeric(substr(base[1,1],5,6))
#       fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#     
#     colnames(fgv_ts)<-paste0("ST_",codigos)
#   }
#   
#   codigos_info <- paste0("ST_",codigos)
#   
#   
#   list(dados =fgv_ts,nome=tabela[,"Nome"],codigo = codigos_info)
#   
# })
# 
# # Mostrar gráficos IPC
# output$grafico_ipc <- renderDygraph({
#   # Encontrar n
#   n <- length(dados_ipc()$nome)
#   # Fazer o gráfico
#   if(n == 1){    
#     dygraph(dados_ipc()$dados, main = "") %>%
#       dySeries("V1", label = dados_ipc()$codigo, strokeWidth = 2) %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#       dyRangeSelector()
#   }else{
#     dygraph(dados_ipc()$dados, main = "") %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#       dyRangeSelector()
#   }
# })
# 
# output$dados_ipc<-renderTable({
#   
#   data.frame(dados_ipc()$dados)
#   
# })
# 
# 
# # Mostrar legenda IPA-10
# output$legenda_ipc <- renderTable({
#   tabela <- cbind(dados_ipc()$codigo,as.character(dados_ipc()$nome))
#   colnames(tabela) <- c("Código", "Nome")
#   tabela
# })
# 
# # Mostrar estatísticas descritivas IPc
# output$descritiva_ipc <- renderTable({
#   
#   n <- length(dados_ipc()$nome)
#   media <- 0
#   mediana <- 0
#   mini <- 0 
#   maxi <- 0
#   
#   if(n > 1){
#     for(i in 1:n){
#       media[i] <- mean(dados_ipc()$dados[,i], na.rm = T)
#       mediana[i] <- median(dados_ipc()$dados[,i], na.rm = T)
#       mini[i] <- min(dados_ipc()$dados[,i], na.rm = T)
#       maxi[i] <- max(dados_ipc()$dados[,i], na.rm = T)
#     }}else{
#       media <- mean(dados_ipc()$dados, na.rm = T)
#       mediana <- median(dados_ipc()$dados, na.rm = T)
#       mini <- min(dados_ipc()$dados, na.rm = T)
#       maxi <- max(dados_ipc()$dados, na.rm = T)
#     }
#   
#   tabela <- matrix(NA, ncol = 4, nrow = n)
#   colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#   rownames(tabela) <- dados_ipc()$nome
#   tabela[,1] <- media
#   tabela[,2] <- mediana
#   tabela[,3] <- mini
#   tabela[,4] <- maxi
#   tabela
# })
# 
# 
# 
# 
# 
# ############ Fim IPC ##########################################################################
# 
# #####################################INCC --------------------------------------------------------------------
# 
# # INCC - 10 ---------------------------------------
# 
# filtro_incc10<-reactive({
#   masc_estrutura<-subset_incc$tipo=="10"
#   masc_estagio <- subset_incc$Estagio == input$incc10_estagios
#   masc_grupo <- subset_incc$Grupo == input$incc10_grupos
#   masc_subgrupo <- subset_incc$Subgrupo == input$incc10_subgrupos
#   masc_item <- subset_incc$Item == input$incc10_itens
#   
#   if(input$incc10_estagios == "TODOS OS ESTÁGIOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_grupo == 0  & subset_incc$Cod_subgrupo == 0 & subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_tudo,]
#   }else if(input$incc10_grupos == "TODOS OS GRUPOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_subgrupo == 0 & subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estagio & masc_tudo,] 
#   }else if(input$incc10_subgrupos == "TODOS OS SUBGRUPOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estagio & masc_grupo & masc_tudo,] 
#   }else if(input$incc10_itens == "TODOS OS ITENS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,] 
#   }else{subset_incc[masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_item,]}
#   #subset_incc[masc_estagio & masc_grupo & masc_subgrupo & masc_item,]
# })
# 
# output$filtro_incc10<-renderDataTable(
#   
#   if(input$visu_incc10 == 0){ return("")
#   }else{
#     isolate({
#       
#       # barrinha carregando
#       withProgress(message = 'Carregando:', value = 0, {
#         n <- 100
#         for (i in 1:n) {
#           incProgress(1/n, detail = paste0(i,"%"))
#           Sys.sleep(0.001) 
#         }
#       })
#   
#     datatable(filtro_incc10()[,c("Código", "Nome","Periodicidade","Início","Fim")], 
#               rownames = filtro_incc10()[,"Código"],
#               options = list(lengthMenu = c(5, 10, 20),
#                              pageLength = 10))
#     })}
#   
#   )
# 
# 
# # BOTÃO VISUALIZAR SÉRIES INCC --------------------------------------- #
# # código/linhas/séries selecionadas na tabela ipa10
# dados_incc10 <- reactive({
#   #linhas selecionadas
#   codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#   codigos <- gsub(" ", "", codigos, fixed = TRUE)
#   codigos <-as.character(filtro_incc10()[input$filtro_incc10_rows_selected, "Código"])
#   codigos_fgv <- paste0("ST_",codigos)
#   tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#   # list(dados = FGV[,codigos_fgv], nome = tabela[,"Nome"], codigo = codigos_fgv)
#   #codigos<-as.numeric(input$filtro_incc10_rows_selected)
#   
#   palavras<-as.character(codigos)
#   strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#   dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#   FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#   odbcCloseAll()
#   
#   str(FGV_base)
#   
#   ######## indo recortar a base ################################
#   base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#   start_ano=as.numeric(substr(base[1,1],1,4))
#   start_mes=as.numeric(substr(base[1,1],5,6))
#   
#   fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#   
#   if(length(codigos)>1){
#     for (i in 2:length(codigos)){
#       base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#       start_ano=as.numeric(substr(base[1,1],1,4))
#       start_mes=as.numeric(substr(base[1,1],5,6))
#       fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#     
#     colnames(fgv_ts)<-paste0("ST_",codigos)
#   }
#   tabela <- table_igp()[input$filtro_incc10_rows_selected, c("Código","Nome")]
#   codigos_info <- paste0("ST_",codigos)
#   list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
# })
# 
# 
# # Mostrar gráficos INCC
# output$grafico_incc10 <- renderDygraph({
#   # Encontrar n
#   n <- length(dados_incc10()$nome)
#   # Fazer o gráfico
#   if(n == 1){    
#     dygraph(dados_incc10()$dados, main = "") %>%
#       dySeries("V1", label = dados_incc10()$codigo, strokeWidth = 2) %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#       dyRangeSelector()
#   }else{
#     dygraph(dados_incc10()$dados, main = "") %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#       dyRangeSelector()
#   }
# })
# 
# 
# output$dados_incc10<-renderTable({
#   
#   data.frame(dados_incc10()$dados)
#   
# })
# 
# 
# output$legenda_incc10 <- renderTable({
#   tabela <- cbind(dados_incc10()$codigo,as.character(dados_incc10()$nome))
#   colnames(tabela) <- c("Código", "Nome")
#   tabela
# })
# 
# 
# 
# 
# 
# 
# # Mostrar estatísticas descritivas incc10
# output$descritiva_incc10 <- renderTable({
#   
#   n <- length(dados_incc10()$nome)
#   media <- 0
#   mediana <- 0
#   mini <- 0 
#   maxi <- 0
#   
#   if(n > 1){
#     for(i in 1:n){
#       media[i] <- mean(dados_incc10()$dados[,i], na.rm = T)
#       mediana[i] <- median(dados_incc10()$dados[,i], na.rm = T)
#       mini[i] <- min(dados_incc10()$dados[,i], na.rm = T)
#       maxi[i] <- max(dados_incc10()$dados[,i], na.rm = T)
#     }}else{
#       media <- mean(dados_incc10()$dados, na.rm = T)
#       mediana <- median(dados_incc10()$dados, na.rm = T)
#       mini <- min(dados_incc10()$dados, na.rm = T)
#       maxi <- max(dados_incc10()$dados, na.rm = T)
#     }
#   
#   tabela <- matrix(NA, ncol = 4, nrow = n)
#   colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#   rownames(tabela) <- dados_incc10()$nome
#   tabela[,1] <- media
#   tabela[,2] <- mediana
#   tabela[,3] <- mini
#   tabela[,4] <- maxi
#   tabela
# })
# 
# 
# #####################################INCC --------------------------------------------------------------------
# 
# # INCC - M ---------------------------------------
# 
# filtro_inccm<-reactive({
#   masc_estrutura<-subset_incc$tipo=="M"
#   masc_estagio <- subset_incc$Estagio == input$inccm_estagios
#   masc_grupo <- subset_incc$Grupo == input$inccm_grupos
#   masc_subgrupo <- subset_incc$Subgrupo == input$inccm_subgrupos
#   masc_item <- subset_incc$Item == input$inccm_itens
#   
#   if(input$inccm_estagios == "TODOS OS ESTÁGIOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_grupo == 0  & subset_incc$Cod_subgrupo == 0 & subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_tudo,]
#   }else if(input$inccm_grupos == "TODOS OS GRUPOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_subgrupo == 0 & subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estagio & masc_tudo,] 
#   }else if(input$inccm_subgrupos == "TODOS OS SUBGRUPOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estagio & masc_grupo & masc_tudo,] 
#   }else if(input$inccm_itens == "TODOS OS ITENS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,] 
#   }else{subset_incc[masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_item,]}
#   #subset_incc[masc_estagio & masc_grupo & masc_subgrupo & masc_item,]
# })
# 
# output$filtro_inccm<-renderDataTable(
#   
#   if(input$visu_inccm == 0){ return("")
#   }else{
#     isolate({
#       
#       # barrinha carregando
#       withProgress(message = 'Carregando:', value = 0, {
#         n <- 100
#         for (i in 1:n) {
#           incProgress(1/n, detail = paste0(i,"%"))
#           Sys.sleep(0.001) 
#         }
#       })
#       
#       datatable(filtro_inccm()[,c("Código","Nome","Periodicidade","Início","Fim")], 
#                 rownames = filtro_inccm()[,"Código"],
#                 options = list(lengthMenu = c(5, 10, 20),
#                                pageLength = 10))
#     })}
#   
# )
# 
# 
# # BOTÃO VISUALIZAR SÉRIES INCC --------------------------------------- #
# # código/linhas/séries selecionadas na tabela ipa10
# dados_inccm <- reactive({
#   #linhas selecionadas
#   codigos <- input$filtro_inccm_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#   codigos <- gsub(" ", "", codigos, fixed = TRUE)
#   codigos <-as.character(filtro_inccm()[input$filtro_inccm_rows_selected, "Código"])
#   codigos_fgv <- paste0("ST_",codigos)
#   tabela <- filtro_inccm()[filtro_inccm()[,"Código"] %in% codigos, c("Código","Nome")]
#   # list(dados = FGV[,codigos_fgv], nome = tabela[,"Nome"], codigo = codigos_fgv)
#   #codigos<-as.numeric(input$filtro_inccm_rows_selected)
#   
#   palavras<-as.character(codigos)
#   strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#   dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#   FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#   odbcCloseAll()
#   
#   str(FGV_base)
#   
#   ######## indo recortar a base ################################
#   base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#   start_ano=as.numeric(substr(base[1,1],1,4))
#   start_mes=as.numeric(substr(base[1,1],5,6))
#   
#   fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#   
#   if(length(codigos)>1){
#     for (i in 2:length(codigos)){
#       base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#       start_ano=as.numeric(substr(base[1,1],1,4))
#       start_mes=as.numeric(substr(base[1,1],5,6))
#       fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#     
#     colnames(fgv_ts)<-paste0("ST_",codigos)
#   }
#   tabela <- table_igp()[input$filtro_inccm_rows_selected, c("Código","Nome")]
#   codigos_info <- paste0("ST_",codigos)
#   list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
# })
# 
# 
# # Mostrar gráficos INCC
# output$grafico_inccm <- renderDygraph({
#   # Encontrar n
#   n <- length(dados_inccm()$nome)
#   # Fazer o gráfico
#   if(n == 1){    
#     dygraph(dados_inccm()$dados, main = "") %>%
#       dySeries("V1", label = dados_inccm()$codigo, strokeWidth = 2) %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#       dyRangeSelector()
#   }else{
#     dygraph(dados_inccm()$dados, main = "") %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#       dyRangeSelector()
#   }
# })
# 
# 
# output$dados_inccm<-renderTable({
#   
#   data.frame(dados_inccm()$dados)
#   
# })
# 
# # Mostrar estatísticas descritivas inccm
# output$descritiva_inccm <- renderTable({
#   
#   n <- length(dados_inccm()$nome)
#   media <- 0
#   mediana <- 0
#   mini <- 0 
#   maxi <- 0
#   
#   if(n > 1){
#     for(i in 1:n){
#       media[i] <- mean(dados_inccm()$dados[,i], na.rm = T)
#       mediana[i] <- median(dados_inccm()$dados[,i], na.rm = T)
#       mini[i] <- min(dados_inccm()$dados[,i], na.rm = T)
#       maxi[i] <- max(dados_inccm()$dados[,i], na.rm = T)
#     }}else{
#       media <- mean(dados_inccm()$dados, na.rm = T)
#       mediana <- median(dados_inccm()$dados, na.rm = T)
#       mini <- min(dados_inccm()$dados, na.rm = T)
#       maxi <- max(dados_inccm()$dados, na.rm = T)
#     }
#   
#   tabela <- matrix(NA, ncol = 4, nrow = n)
#   colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#   rownames(tabela) <- dados_inccm()$nome
#   tabela[,1] <- media
#   tabela[,2] <- mediana
#   tabela[,3] <- mini
#   tabela[,4] <- maxi
#   tabela
# })
# 
# #####################################INCC --------------------------------------------------------------------
# 
# # INCC - DI ---------------------------------------
# 
# filtro_inccdi<-reactive({
#   masc_estrutura<-subset_incc$tipo=="DI"
#   masc_estagio <- subset_incc$Estagio == input$inccdi_estagios
#   masc_grupo <- subset_incc$Grupo == input$inccdi_grupos
#   masc_subgrupo <- subset_incc$Subgrupo == input$inccdi_subgrupos
#   masc_item <- subset_incc$Item == input$inccdi_itens
#   
#   if(input$inccdi_estagios == "TODOS OS ESTÁGIOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_grupo == 0  & subset_incc$Cod_subgrupo == 0 & subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_tudo,]
#   }else if(input$inccdi_grupos == "TODOS OS GRUPOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_subgrupo == 0 & subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estagio & masc_tudo,] 
#   }else if(input$inccdi_subgrupos == "TODOS OS SUBGRUPOS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_item == 0 & subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estagio & masc_grupo & masc_tudo,] 
#   }else if(input$inccdi_itens == "TODOS OS ITENS (MAIOR NÍVEL)"){
#     masc_tudo <- subset_incc$Cod_subitem == 0
#     subset_incc[masc_estrutura & masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_tudo,] 
#   }else{subset_incc[masc_estrutura & masc_estagio & masc_grupo & masc_subgrupo & masc_item,]}
#   #subset_incc[masc_estagio & masc_grupo & masc_subgrupo & masc_item,]
# })
# 
# output$filtro_inccdi<-renderDataTable(
#   
#   if(input$visu_inccdi == 0){ return("")
#   }else{
#     isolate({
#       
#       # barrinha carregando
#       withProgress(message = 'Carregando:', value = 0, {
#         n <- 100
#         for (i in 1:n) {
#           incProgress(1/n, detail = paste0(i,"%"))
#           Sys.sleep(0.001) 
#         }
#       })
#       
#       datatable(filtro_inccdi()[,c("Código","Nome","Periodicidade","Início","Fim")], 
#                 rownames = filtro_inccdi()[,"Código"],
#                 options = list(lengthMenu = c(5, 10, 20),
#                                pageLength = 10))
#     })}
#   
# )
# 
# 
# # BOTÃO VISUALIZAR SÉRIES INCC --------------------------------------- #
# # código/linhas/séries selecionadas na tabela ipa10
# dados_inccdi <- reactive({
#   #linhas selecionadas
#   codigos <- input$filtro_inccdi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#   codigos <- gsub(" ", "", codigos, fixed = TRUE)
#   codigos <-as.character(filtro_inccdi()[input$filtro_inccdi_rows_selected, "Código"])
#   codigos_fgv <- paste0("ST_",codigos)
#   tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#   #list(dados = FGV[,codigos_fgv], nome = tabela[,"Nome"], codigo = codigos_fgv)
#   #codigos<-as.numeric(input$filtro_inccdi_rows_selected)
#   
#   palavras<-as.character(codigos)
#   strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#   dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#   FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#   odbcCloseAll()
#   
#   str(FGV_base)
#   
#   ######## indo recortar a base ################################
#   base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#   start_ano=as.numeric(substr(base[1,1],1,4))
#   start_mes=as.numeric(substr(base[1,1],5,6))
#   
#   fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#   
#   if(length(codigos)>1){
#     for (i in 2:length(codigos)){
#       base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#       start_ano=as.numeric(substr(base[1,1],1,4))
#       start_mes=as.numeric(substr(base[1,1],5,6))
#       fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#     
#     colnames(fgv_ts)<-paste0("ST_",codigos)
#   }
#   tabela <- table_igp()[input$filtro_inccdi_rows_selected, c("Código","Nome")]
#   codigos_info <- paste0("ST_",codigos)
#   list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
# })
# 
# 
# # Mostrar gráficos INCC
# output$grafico_inccdi <- renderDygraph({
#   # Encontrar n
#   n <- length(dados_inccdi()$nome)
#   # Fazer o gráfico
#   if(n == 1){    
#     dygraph(dados_inccdi()$dados, main = "") %>%
#       dySeries("V1", label = dados_inccdi()$codigo, strokeWidth = 2) %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#       dyRangeSelector()
#   }else{
#     dygraph(dados_inccdi()$dados, main = "") %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#       dyRangeSelector()
#   }
# })
# 
# 
# output$dados_inccdi<-renderTable({
#   
#   data.frame(dados_inccdi()$dados)
#   
# })
# 
# # Mostrar estatísticas descritivas inccdi
# output$descritiva_inccdi <- renderTable({
#   
#   n <- length(dados_inccdi()$nome)
#   media <- 0
#   mediana <- 0
#   mini <- 0 
#   maxi <- 0
#   
#   if(n > 1){
#     for(i in 1:n){
#       media[i] <- mean(dados_inccdi()$dados[,i], na.rm = T)
#       mediana[i] <- median(dados_inccdi()$dados[,i], na.rm = T)
#       mini[i] <- min(dados_inccdi()$dados[,i], na.rm = T)
#       maxi[i] <- max(dados_inccdi()$dados[,i], na.rm = T)
#     }}else{
#       media <- mean(dados_inccdi()$dados, na.rm = T)
#       mediana <- median(dados_inccdi()$dados, na.rm = T)
#       mini <- min(dados_inccdi()$dados, na.rm = T)
#       maxi <- max(dados_inccdi()$dados, na.rm = T)
#     }
#   
#   tabela <- matrix(NA, ncol = 4, nrow = n)
#   colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#   rownames(tabela) <- dados_inccdi()$nome
#   tabela[,1] <- media
#   tabela[,2] <- mediana
#   tabela[,3] <- mini
#   tabela[,4] <- maxi
#   tabela
# })
# 
# #########FIM INCC#############################################################################################
#   
#   ################### SONDAGENS#############################################
#   
#   ########SONDAGEM DA INDUSTRIA ############
#   
#   # dataframe de informações sobre o Industria
#   table_industria <- reactive({
#     
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     # dataframe só com IGP
#     subset(infos[,c("Código","Nome","Periodicidade","Início","Fim")], infos$Serviço== "Industria")
#     
#   })
#   
#   # gerar tabela do dataframe
#   output$tabela_industria <- DT::renderDataTable(
#     datatable(table_industria(), 
#               rownames = table_industria()[,"Código"],
#               options = list(lengthMenu = c(5, 10, 20),
#                              pageLength = 10,
#                              searching = FALSE))            
#   )
#   
#   
#   # código/linhas/séries selecionadas na tabela industria
#   dados_industria <- reactive({
#     ##linhas selecionadas
#     codigos <- input$tabela_industria_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     codigos <-as.character(table_industria()[input$tabela_industria_rows_selected, "Código"])
#     # códigos selecionados
#     tabela <- table_industria()[table_industria()[,"Código"] %in% codigos, c("Código","Nome")]
#     # objeto com as séries temporais
#     codigos_info <- paste0("ST_",codigos)
#     #list(dados = FGV[,codigos_info], nome = tabela[,"Nome"], codigo = codigos_info)
#     #codigos<-as.numeric(input$tabela_industria_rows_selected)
#     
#     palavras<-as.character(codigos)
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela <- table_industria()[input$tabela_industria_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#   })
#   
#   
#   # Mostrar gráficos IGP
#   output$grafico_industria <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_industria()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_industria()$dados, main = "") %>%
#         dySeries("V1", label = dados_industria()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(dados_industria()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   # Mostrar legenda IGP
#   output$legenda_industria<- renderTable({
#     tabela <- cbind(dados_industria()$codigo,dados_industria()$nome)
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#   
#   # Mostrar dados IGP
#   output$dados_industria <- renderTable({
#     dados_industria()$dados
#   })
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_industria <- renderTable({
#     
#     n <- length(dados_industria()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_industria()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_industria()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_industria()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_industria()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_industria()$dados, na.rm = T)
#         mediana <- median(dados_industria()$dados, na.rm = T)
#         mini <- min(dados_industria()$dados, na.rm = T)
#         maxi <- max(dados_industria()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_industria()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   ###############fim Industria###############################
#   
#   
#   ###################comercio#######################################
#   
#   # dataframe de informações sobre o comercio
#   table_comercio <- reactive({
#     
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     # dataframe só com IGP
#     subset(infos[,c("Código","Nome","Periodicidade","Início","Fim")], infos$Serviço=="Comercio")
#     
#   })
#   
#   # gerar tabela do dataframe
#   output$tabela_comercio <- DT::renderDataTable(
#     datatable(table_comercio(), 
#               rownames = table_comercio()[,"Código"],
#               options = list(lengthMenu = c(5, 10, 20),
#                              pageLength = 10,
#                              searching = FALSE))            
#   )
#   
#   
#   
#   ###### capturando dados do comercio ############
#   
#   
#   
#   # código/linhas/séries selecionadas na tabela comercio
#   dados_comercio <- reactive({
#     # linhas selecionadas
#     codigos <- input$tabela_comercio_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     codigos <-as.character(table_comercio()[input$tabela_comercio_rows_selected, "Código"])
#     # códigos selecionados
#     tabela <- table_comercio()[table_comercio()[,"Código"] %in% codigos, c("Código","Nome")]
#     # objeto com as séries temporais
#     codigos_info <- paste0("ST_",codigos)
#     # list(dados = FGV[,codigos_info], nome = tabela[,"Nome"], codigo = codigos_info)
#     
#     #codigos<-as.numeric(input$tabela_comercio_rows_selected)
#     
#     palavras<-as.character(codigos)
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela <- table_comercio()[as.numeric(input$tabela_comercio_rows_selected),c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#   })
#   
#   
#   # Mostrar gráficos comercio
#   output$grafico_comercio <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_comercio()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_comercio()$dados, main = "") %>%
#         dySeries("V1", label = dados_comercio()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(dados_comercio()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   #####mostrar descritiva comercio #########
#   
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_comercio <- renderTable({
#     
#     n <- length(dados_comercio()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_comercio()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_comercio()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_comercio()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_comercio()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_comercio()$dados, na.rm = T)
#         mediana <- median(dados_comercio()$dados, na.rm = T)
#         mini <- min(dados_comercio()$dados, na.rm = T)
#         maxi <- max(dados_comercio()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_comercio()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   # Mostrar legenda IGP
#   output$legenda_comercio<- renderTable({
#     tabela <- cbind(dados_comercio()$codigo,dados_comercio()$nome)
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#   
#   # Mostrar dados IGP
#   output$dados_comercio <- renderTable({
#     dados_comercio()$dados
#   })
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_comercio <- renderTable({
#     
#     n <- length(dados_comercio()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_comercio()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_comercio()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_comercio()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_comercio()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_comercio()$dados, na.rm = T)
#         mediana <- median(dados_comercio()$dados, na.rm = T)
#         mini <- min(dados_comercio()$dados, na.rm = T)
#         maxi <- max(dados_comercio()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_comercio()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   ####################Fim Comercio##############################
#   
#   ################### Servicos ###############################
#   
#   # dataframe de informações sobre o servico
#   table_servico <- reactive({
#     
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     # dataframe só com IGP
#     subset(infos[,c("Código","Nome","Periodicidade","Início","Fim")], infos$Serviço== "Servico")
#     
#   })
#   
#   # gerar tabela do dataframe
#   output$tabela_servico <- DT::renderDataTable(
#     datatable(table_servico(), 
#               rownames = table_servico()[,"Código"],
#               options = list(lengthMenu = c(5, 10, 20),
#                              pageLength = 10,
#                              searching = FALSE))            
#   )
#   
#   
#   # código/linhas/séries selecionadas na tabela servico
#   dados_servico <- reactive({
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.2)
#       }
#     }) 
#     # linhas selecionadas
#     codigos <- input$tabela_servico_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     codigos <-as.character(table_servico()[input$tabela_servico_rows_selected, "Código"])
#     # códigos selecionados
#     tabela <- table_servico()[table_servico()[,"Código"] %in% codigos, c("Código","Nome")]
#     # objeto com as séries temporais
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =na.omit(FGV[,codigos_info]), nome = tabela[,"Nome"], codigo = codigos_info)
#     #codigos<-as.numeric(input$tabela_servico_rows_selected)
#     
#     palavras<-as.character(codigos)
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela <- table_servico()[input$tabela_servico_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#   })
#   
#   
#   # Mostrar gráficos IGP
#   output$grafico_servico <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_servico()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_servico()$dados, main = "") %>%
#         dySeries("V1", label = dados_servico()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(dados_servico()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   # Mostrar legenda IGP
#   output$legenda_servico<- renderTable({
#     tabela <- cbind(dados_servico()$codigo,dados_servico()$nome)
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#   
#   # Mostrar dados IGP
#   output$dados_servico <- renderTable({
#     dados_servico()$dados
#   })
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_servico <- renderTable({
#     
#     n <- length(dados_servico()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_servico()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_servico()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_servico()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_servico()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_servico()$dados, na.rm = T)
#         mediana <- median(dados_servico()$dados, na.rm = T)
#         mini <- min(dados_servico()$dados, na.rm = T)
#         maxi <- max(dados_servico()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_servico()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   
#   ###################FIM SERVICOS ###############################
#   ##################### Construção################################
#   
#   
#   # dataframe de informações sobre o construcao
#   table_construcao <- reactive({
#     
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     # dataframe só com IGP
#     subset(infos[,c("Código","Nome","Periodicidade","Início","Fim")], infos$Serviço== "Construcao")
#     
#   })
#   
#   # gerar tabela do dataframe
#   output$tabela_construcao <- DT::renderDataTable(
#     datatable(table_construcao(), 
#               rownames = table_construcao()[,"Código"],
#               options = list(lengthMenu = c(5, 10, 20),
#                              pageLength = 10,
#                              searching = FALSE))            
#   )
#   
#   
#   # código/linhas/séries selecionadas na tabela construcao
#   dados_construcao <- reactive({
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.2)
#       }
#     }) 
#     # linhas selecionadas
#     codigos <- input$tabela_construcao_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     # códigos selecionados
#     tabela <- table_construcao()[table_construcao()[,"Código"] %in% codigos, c("Código","Nome")]
#     codigos <-as.character(table_construcao()[input$tabela_construcao_rows_selected, "Código"])
#     # objeto com as séries temporais
#     codigos_info <- paste0("ST_",codigos)
#     # list(dados = FGV[,codigos_info], nome = tabela[,"Nome"], codigo = codigos_info)
#     #codigos<-as.numeric(input$tabela_construcao_rows_selected)
#     
#     palavras<-as.character(codigos)
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela <- table_construcao()[input$tabela_construcao_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = tabela[,"Nome"], codigo = codigos_info)
#   })
#   
#   
#   # Mostrar gráficos IGP
#   output$grafico_construcao <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_construcao()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_construcao()$dados, main = "") %>%
#         dySeries("V1", label = dados_construcao()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(dados_construcao()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   # Mostrar legenda IGP
#   output$legenda_construcao<- renderTable({
#     tabela <- cbind(dados_construcao()$codigo,dados_construcao()$nome)
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#   
#   # Mostrar dados IGP
#   output$dados_construcao <- renderTable({
#     dados_construcao()$dados
#   })
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_construcao <- renderTable({
#     
#     n <- length(dados_construcao()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_construcao()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_construcao()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_construcao()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_construcao()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_construcao()$dados, na.rm = T)
#         mediana <- median(dados_construcao()$dados, na.rm = T)
#         mini <- min(dados_construcao()$dados, na.rm = T)
#         maxi <- max(dados_construcao()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_construcao()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   
#   ############### Fim Construção################################
# 
#   ################### Consumidor ###############################
#   
#   # dataframe de informações sobre o consumidor
#   table_consumidor <- reactive({
#     
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.01)
#       }
#     }) # fim barrinha carregando
#     
#     # dataframe só com IGP
#     subset(infos[,c("Código","Nome","Periodicidade","Início","Fim")], infos$Serviço== "Consumidor")
#     
#   })
#   
#   # gerar tabela do dataframe
#   output$tabela_consumidor <- DT::renderDataTable(
#     datatable(table_consumidor(), 
#               rownames = table_consumidor()[,"Código"],
#               options = list(lengthMenu = c(5, 10, 20),
#                              pageLength = 10,
#                              searching = FALSE))            
#   )
#   
#   
#   # código/linhas/séries selecionadas na tabela consumidor
#   dados_consumidor <- reactive({
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.2)
#       }
#     }) 
#     # linhas selecionadas
#     codigos <- input$tabela_consumidor_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#     codigos <- gsub(" ", "", codigos, fixed = TRUE)
#     # códigos selecionados
#     codigos <-as.character(table_consumidor()[input$tabela_consumidor_rows_selected, "Código"])
#     tabela <- table_consumidor()[table_consumidor()[,"Código"] %in% codigos, c("Código","Nome")]
#     # objeto com as séries temporais
#     codigos_info <- paste0("ST_",codigos)
#     list(dados = FGV[,codigos_info], nome = tabela[,"Nome"], codigo = codigos_info)
#     #codigos<-as.numeric(input$tabela_consumidor_rows_selected)
#     
#     palavras<-as.character(codigos)
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
#     
#     str(FGV_base)
#     
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
#     
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
#     
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
#       
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     tabela<-table_consumidor()[input$tabela_consumidor_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     list(dados =fgv_ts, nome = as.character(tabela[,"Nome"]), codigo = codigos_info)
#   })
#   
#   
#   # Mostrar gráficos IGP
#   output$grafico_consumidor <- renderDygraph({
#     # Encontrar n
#     n <- length(dados_consumidor()$nome)
#     # Fazer o gráfico
#     if(n == 1){    
#       dygraph(dados_consumidor()$dados, main = "") %>%
#         dySeries("V1", label = dados_consumidor()$codigo, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(dados_consumidor()$dados, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()
#     }
#   })
#   
#   # Mostrar legenda IGP
#   output$legenda_consumidor<- renderTable({
#     tabela <- data.frame(cbind(dados_consumidor()$codigo),dados_consumidor()$nome)
#     names(tabela) <- c("Código","Nome")
#     tabela
#   })
#   
#   # Mostrar dados IGP
#   output$dados_consumidor <- renderTable({
#     dados_consumidor()$dados
#   })
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_consumidor <- renderTable({
#     
#     n <- length(dados_consumidor()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_consumidor()$dados[,i], na.rm = T)
#         mediana[i] <- median(dados_consumidor()$dados[,i], na.rm = T)
#         mini[i] <- min(dados_consumidor()$dados[,i], na.rm = T)
#         maxi[i] <- max(dados_consumidor()$dados[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_consumidor()$dados, na.rm = T)
#         mediana <- median(dados_consumidor()$dados, na.rm = T)
#         mini <- min(dados_consumidor()$dados, na.rm = T)
#         maxi <- max(dados_consumidor()$dados, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_consumidor()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   
#   ##################### Fim Consumidor ####################################
# 
#  # BANCO ST: BRASIL ----------------------------------------------------------
#  
#   
#   # Banco ST: Brasil - IBGE -----------------------------
#   
#   output$tabela_IBGE<- DT::renderDataTable(datatable(base_IBGE[,c(1,2,4,5,6)], 
#                         rownames = base_IBGE[,"Cód."], 
#                         #rownames =TRUE,
#                         #selection = 'single',
#                         options = list(pageLength = 5))                 
#   )
#   
#   libera_ibge<-reactive({
#     periodicidade=subset(base_IBGE,base_IBGE$Cód. %in% as.numeric(base_IBGE[input$tabela_IBGE_rows_selected,1]))[,4]
#     if(length(unique(periodicidade))==1){
#       libera<-0
#     }else{
#       libera<-1
#     }
#     libera
#   })
#   
#   output$liberaibge <- reactive({
#     
#     if(libera_ibge()== 0){condicao = 0}else{condicao = 1}
#     condicao
#     
#   })
#   
#   
#   outputOptions(output,'liberaibge', suspendWhenHidden=FALSE)
# 
#   completo<- reactive({
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.2)
#       }
#     }) 
#      # linhas<-input$tabela_IBGE_rows_selected
#      #  api_ibge(linhas)
#     linhas<-as.numeric(as.character(base_IBGE[input$tabela_IBGE_rows_selected,1]))
#     periodicidade=subset(base_IBGE,base_IBGE$Cód. %in% base_IBGE[input$tabela_IBGE_rows_selected,1] )[1,4]
#     ts=BETS.get(linhas,periodicidade=periodicidade)[[1]]
#     if(!is.null(dim(ts))){
#       colnames(ts)<-subset(base_IBGE,base_IBGE$Cód. %in% base_IBGE[input$tabela_IBGE_rows_selected,1])[,2]
#     }
#     list(ts=ts,data=as.character(BETS.get(linhas,periodicidade=periodicidade)[[2]]),nome=subset(base_IBGE,base_IBGE$Cód. %in% linhas)[,2],cod=linhas,periodicidade=periodicidade)
#     })
#   
#   # 
#   # output$teste_erro<-renderText({
#   #   completo()$erro
#   # })
# 
# #   output$erroibge<-renderText({
# #             if (completo()$erro>0){ 
# #               createAlert(session, "alert", "exampleAlert", title = "Oops",
# #                           content = "Both inputs should be numeric.", append = FALSE)
# #             }else{
# #               closeAlert(session, "exampleAlert")
# #             }
# #           return("")
# #   })
#   
#   output$erro<-renderText({
# 
#     completo()$erro=0
# 
#   })
# #   
# # 
# #   outputOptions(output, "erro", suspendWhenHidden=FALSE)
#   
#   #######grafico das series temporais do IBGE #########################
#   
#   output$grafico <- renderDygraph({
#     
#     # 
#   if (completo()$periodicidade!="A"){
#       mes_final <- end(completo()$ts)[2]
#       ano_final <- end(completo()$ts)[1]
#       mes_inicial <- start(completo()$ts)[2]
#       ano_inicial <- start(completo()$ts)[1]
# 
#      #  dygraph(completo()$ts, main ="")%>% #completo()$nome) %>%
#      # dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"))# %>%
#      #    dyRangeSelector(fillColor = "#2C65A1",
#      #                     dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#      #                                    paste0(ano_final,"-",mes_final,"-01")))
#      #Encontrar n
#     n <- length(completo()$data)
#     # Fazer o gráfico
#     if(n == 1){
#       dygraph(completo()$ts, main = "") %>%
#         dySeries("V1", label = completo()$cod, strokeWidth = 2) %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyRangeSelector()
#     }else{
#       dygraph(completo()$ts, main = "") %>%
#         dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#         dyRangeSelector()}
#     }
# 
#     else if(completo()$periodicidade=="A"){
# 
#       if(!is.null(dim(completo()$ts))){
#         ts<-xts(as.numeric(completo()$ts[,1]),as.Date(completo()$ts))
#         #nomes<-colnames(completo()$ts)
#         nomes<-completo()$nome
#         for(i in 2:dim(completo()$ts)[2]){
#           ts<-cbind(ts,xts(as.numeric(completo()$ts[,i]),as.Date(completo()$ts)))
#           }
#         colnames(ts)<-nomes
#         }
#       else if(is.null(dim(completo()$ts))){
#         ts<-xts(as.numeric(completo()$ts),as.Date(completo()$ts))
#       }
# 
#       ts<-na.omit(ts)
#       mes_final <- end(ts)[2]
#       ano_final <- end(ts)[1]
#       mes_inicial <- start(ts)[2]
#       ano_inicial <- start(ts)[1]
#       dygraph(ts, main ="Séries IBGE") %>%
#         #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyOptions(connectSeparatedPoints = TRUE)%>%
#         dyRangeSelector(fillColor = "#2C65A1",
#                         dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                        paste0(ano_final,"-",mes_final,"-01")))
#     }
# 
#     
#   })
#   
#   ######dados do IBGE
#   
#   output$pirint<-renderTable({
#     # df_index<-data.frame(completo()$data)
#     # colnames(df_index)<-"Data"
#     # df<-data.frame(completo()$ts)
#     # #     lines$data<-paste0(lines$mesof,"/",lines$anouso)
#     # #     frame_ibge$data<-lines$data
#     # #     frame_ibge<-frame_ibge[,c(2,1)]
#     # df<-cbind(df_index,df)
#     # df
#     # df<-data.frame(completo()$ts)
#     # #name<-as.character(subset(base_IBGE,base_IBGE$Cód. %in% input$tabela_rows_selected)[,2])
#     # name<-completo()$nome
#     # df2<-data.frame(completo()$data)
#     # df<-cbind(df2,df)
#     # names(df)<-c("Data",name)
#     # #data.frame(input$tabela_rows_selected)
#     # #data.frame(dados_()$data)
#     # row.names(df)<-df$Data
#     # df[,-1]
#     
#     data.frame(subset(base_IBGE,base_IBGE$Cód. %in% as.numeric(base_IBGE[input$tabela_IBGE_rows_selected,1]))[,4])
#     
#     #data.frame(completo()$periodicidade!="A")
#   
#   #data.frame(subset(base_IBGE,base_IBGE$Cód. %in% as.numeric(as.character(input$tabela_IBGE_rows_selected))))
#     
#   })
#   # 
#   # output$ingrid<-renderPrint({
#   #   
#   #   # ts<-xts(as.numeric(completo()$ts[,1]),as.Date(completo()$ts))
#   #   # #nomes<-colnames(completo()$ts)
#   #   # nomes<-completo()$nome
#   #   # 
#   #   # if(!is.null(dim(completo()$ts))){
#   #   #   for(i in 2:dim(completo()$ts)[2]){
#   #   #     ts<-cbind(ts,xts(as.numeric(completo()$ts[,i]),as.Date(completo()$ts)))}
#   #   #   colnames(ts)<-nomes}
#   #   # 
#   #   completo()$ts
#   #   
#   #   
#   # })
#   
#   ######estatísticas descritivas IBGE
#   
#   output$descritiva_stIBGE <- renderTable({
#     
#     # media <- 0
#     # mediana <- 0
#     # mini <- 0 
#     # maxi <- 0
#     # 
#     # media <- mean(completo()$ts, na.rm = T)
#     # mediana <- median(completo()$ts, na.rm = T)
#     # mini <- min(completo()$ts, na.rm = T)
#     # maxi <- max(completo()$ts, na.rm = T)
#     # 
#     # tabela <- matrix(NA, ncol = 4, nrow = 1)
#     # colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     # rownames(tabela) <- completo()$codigo
#     # tabela[,1] <- media
#     # tabela[,2] <- mediana
#     # tabela[,3] <- mini
#     # tabela[,4] <- maxi
#     # tabela
#     
#     
#     n <- length(completo()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(as.numeric(completo()$ts[,i]), na.rm = T)
#         mediana[i] <- median(as.numeric(completo()$ts[,i]), na.rm = T)
#         mini[i] <- min(as.numeric(completo()$ts[,i]), na.rm = T)
#         maxi[i] <- max(as.numeric(completo()$ts[,i]), na.rm = T)
#       }}else{
#         media <- mean(as.numeric(completo()$ts), na.rm = T)
#         mediana <- median(as.numeric(completo()$ts), na.rm = T)
#         mini <- min(as.numeric(completo()$ts), na.rm = T)
#         maxi <- max(as.numeric(completo()$ts), na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- completo()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#     
#   })
#   
#   # Câmbio Dólar/Real --------------------------------------------------------
#   brasil_cambio <- reactive({
#     getSymbols("BRL=X", src="yahoo", auto.assign=FALSE, warnings = FALSE)
#   })
#   
#   output$brasil_cambio_data <- renderText({
#     data <- strsplit(as.character(index(brasil_cambio()[dim(brasil_cambio())[1],])),split = "-" )[[1]]
#     data <- paste0(data[3],"/",data[2],"/",data[1])
#     data
#   })
#   output$brasil_cambio_Open <-  renderText(brasil_cambio()[dim(brasil_cambio())[1],"BRL=X.Open"])
#   output$brasil_cambio_Close <- renderText(brasil_cambio()[dim(brasil_cambio())[1],"BRL=X.Close"])
#   output$brasil_cambio_High <- renderText(brasil_cambio()[dim(brasil_cambio())[1],"BRL=X.High"])
#   output$brasil_cambio_Low <- renderText(brasil_cambio()[dim(brasil_cambio())[1],"BRL=X.Low"])
#   
#   output$brasil_cambio_grafico <- renderDygraph({
#     # encontrar as datas iniciais e finais da série temporal
#     inicio <- start(brasil_cambio())
#     fim <- end(brasil_cambio())
#     
#     dygraph(brasil_cambio()[,"BRL=X.Close"], main = "Câmbio Dólar/Real - Close") %>%
#       dyOptions(colors = "#2C65A1") %>% 
#       dyRangeSelector(fillColor = "#2C65A1",
#                       dateWindow = c(inicio,fim))
#   })
#   
#   
#  # BANCO ST: AMÉRICA LATINA---------------------------------------------------
#   # lista de séries disponíveis
#   mundo_al <- reactive({
#     subset(cod_series_mundo, cod_series_mundo$LOCAL == "AL")
#   })
#   
#   output$tabela_mundo_al <- DT::renderDataTable({
# #     datatable(mundo_al()[,c("NOME","Periodicidade","Fonte")], 
# #               #rownames = mundo_al()[,"COD"], 
# #               selection = 'single',
# #               options = list(pageLength = 5)) 
#     
#     datatable(mundo_al()[,c("COD","NOME","Periodicidade","Fonte","Data.Final","Data.Inicial")], 
#               rownames = mundo_al()[,"COD"], 
#               #rownames =TRUE,
#               selection = 'single',
#               options = list(pageLength = 5))            
#   })
#   
# 
#   
#   # BOTÃO VISUALIZAR SÉRIES MUNDO EUA
#   visu_al <- reactive({
#     # barrinha carregando
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.005)
#       }
#     }) # fim barrinha carregando
#     
#     sel1 <- mundo_al()[input$tabela_mundo_al_rows_selected,1]
#     n <- length(sel1)
#     sel <- sel1[n]
#     nome <- mundo_al()$NOME[mundo_al()$COD == sel]
#     cod_nome <- mundo_al()$COD_NOME[mundo_al()$COD == sel]
#     src <- mundo_al()$SRC[mundo_al()$COD == sel]
#     coluna <- mundo_al()$COLUNA[mundo_al()$COD == sel]
#     dados <- getSymbols(cod_nome, src=src, auto.assign=FALSE, warnings = FALSE)[,coluna]
#     list(nome = nome, dados = dados)
#   })
#   
#   # Mostrar gráfico
#   output$grafico_stmundo_al <- renderDygraph({
#     dygraph(visu_al()$dados, main = visu_al()$nome) %>%
#       dyOptions(colors = "dodgerblue") %>%
#       dyRangeSelector(fillColor = "#2C65A1")
#   })
#   
#   # Mostrar dados 
#   output$dados_stmundo_al <- renderTable({
#     data <- index(visu_al()$dados)
#     frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_al()$dados))
#     colnames(frame)[2] <- visu_al()$nome
#     frame
#   })
#   
#   # Mostrar estatísticas descritivas séries mundo
#   output$descritiva_stmundo_al <- renderTable({
#     
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     media <- mean(visu_al()$dados, na.rm = T)
#     mediana <- median(visu_al()$dados, na.rm = T)
#     mini <- min(visu_al()$dados, na.rm = T)
#     maxi <- max(visu_al()$dados, na.rm = T)
#     
#     tabela <- matrix(NA, ncol = 4, nrow = 1)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- visu_al()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   ###### series europa###################
#   
#   
#   
#  
#   # Banco ST: Brasil - Banco Central -----------------------------
#   
#   output$bacen_grupo<- renderUI({
#     #subset_ipa_estrutura <- subset(subset_ipa, subset_ipa$Estrutura == ipa_estrutura())
#     nomes <- c("TODOS OS GRUPOS",unique(base_bacen$grupo)) 
#     selectInput("bacen_grupos", label = "1 - Grupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   
#   output$bacen_subgrupo<- renderUI({
#     subset_bacen<-subset(base_bacen,base_bacen$grupo==input$bacen_grupos)
#       nomes <- c("TODOS OS SUBGRUPOS",unique(as.character(subset_bacen$subgrupo))) 
#     selectInput("bacen_subgrupo", label = "2 - Subgrupos:", width = "100%", 
#                 choices = nomes)
#   })
#   
#   
#   bacen_grupo<-reactive({
#     data.frame(subset(base_bacen,base_bacen$subgrupo==input$bacen_subgrupo))
#   })
#   
#   output$bacentabela<- DT::renderDataTable({
#     datatable(bacen_grupo()[,c(1,2,4,5,6)], 
#               rownames = base_bacen[,"Cód."], 
#               #rownames =TRUE,
#               #selection = 'single',
#               options = list(pageLength = 5))            
#   })
#   
#   libera_bacen<-reactive({
#     periodicidade=subset(base_bacen,base_bacen$Cód. %in% bacen_grupo()[input$bacentabela_rows_selected,1])[,4]
#     if(length(unique(periodicidade))==1){
#       libera<-0
#     }else{
#       libera<-1
#     }
#     libera
#   })
#   
#   output$liberabacen <- reactive({
#     
#     if(libera_bacen()== 0){condicao = 0}else{condicao = 1}
#     condicao
#     
#   })
#   outputOptions(output, 'liberabacen', suspendWhenHidden=FALSE)
#   
#   dados_bacen<- reactive({
#     withProgress(message = 'Carregando:', value = 0, {
#       n <- 100
#       for (i in 1:n) {
#         incProgress(1/n, detail = paste0(i,"%"))
#         # Pause for 0.02 seconds to simulate a long computation.
#         Sys.sleep(0.2)
#       }
#     }) 
#       
#     linhas<-as.numeric(as.character(bacen_grupo()[input$bacentabela_rows_selected,1]))
#     periodicidade=subset(base_bacen,base_bacen$Cód. %in% linhas )[1,4]
#     ts=BETS.get(linhas,periodicidade=periodicidade)[[1]]
#     if(!is.null(dim(ts))){
#       colnames(ts)<-subset(base_bacen,base_bacen$Cód. %in% bacen_grupo()[input$bacentabela_rows_selected,1])[,2]
#     }
#     list(ts=ts,data=as.character(BETS.get(linhas,periodicidade=periodicidade)[[2]]),nome=subset(base_bacen,base_bacen$Cód. %in% linhas)[,2],cod=linhas,periodicidade=periodicidade)
# 
#     })
#   
#   
#   output$dados_bacen<-renderTable({
#     df<-data.frame(dados_bacen()$ts)
#     name<-subset(base_bacen,base_bacen$Cód. %in% input$tabela_igp_rows_selected)[,2]
#     df2<-data.frame(dados_bacen()$data)
#     df<-cbind(df2,df)
#     names(df)<-c("Data",name)
#     row.names(df)<-df$Data
#     df[,-1]
#   })
#   
#   
#   output$teste_bacen<-renderPrint({
#     
#    
#     if(dados_bacen()$periodicidade!="A"){
#       mes_final <- end(dados_bacen()$ts)[2]
#       ano_final <- end(dados_bacen()$ts)[1]
#       mes_inicial <- start(dados_bacen()$ts)[2]
#       ano_inicial <- start(dados_bacen()$ts)[1]
#     }
#     
#     else if(dados_bacen()$periodicidade=="A"){
#       
#       if(!is.null(dim(dados_bacen()$ts))){
#         ts<-xts(as.numeric(dados_bacen()$ts[,1]),as.Date(dados_bacen()$ts))
#         #nomes<-colnames(completo()$ts)
#         nomes<-dados_bacen()$nome
#         for(i in 2:dim(dados_bacen()$ts)[2]){
#           ts<-cbind(ts,xts(as.numeric(dados_bacen()$ts[,i]),as.Date(dados_bacen()$ts)))
#         }
#         colnames(ts)<-nomes
#       }
#       else if(is.null(dim(dados_bacen()$ts))){
#         ts<-xts(as.numeric(dados_bacen()$ts),as.Date(dados_bacen()$ts))
#       }
#       
#       ts<-na.omit(ts)
#       mes_final <- end(ts)[2]
#       ano_final <- end(ts)[1]
#       mes_inicial <- start(ts)[2]
#       ano_inicial <- start(ts)[1]}
# 
#     nomes
#   })
#   
#   # Mostrar gráficos IGP
#   output$grafico_bacen <- renderDygraph({
#    
#     if(dados_bacen()$periodicidade!="A"){
#       mes_final <- end(dados_bacen()$ts)[2]
#       ano_final <- end(dados_bacen()$ts)[1]
#       mes_inicial <- start(dados_bacen()$ts)[2]
#       ano_inicial <- start(dados_bacen()$ts)[1]
#       
#       #Encontrar n
#       n <- length(dados_bacen()$data)
#       # Fazer o gráfico
#       if(n == 1){
#         dygraph(dados_bacen()$ts, main = "") %>%
#           dySeries("V1", label = dados_bacen()$cod, strokeWidth = 2) %>%
#           dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#           dyRangeSelector()
#       }else{
#         dygraph(dados_bacen()$ts, main = "") %>%
#           dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#           dyRangeSelector()}}
#     
#     else if(dados_bacen()$periodicidade=="A"){
#       
#       if(!is.null(dim(dados_bacen()$ts))){
#         ts<-xts(as.numeric(dados_bacen()$ts[,1]),as.Date(dados_bacen()$ts))
#         #nomes<-colnames(completo()$ts)
#         nomes<-dados_bacen()$nome
#         for(i in 2:dim(dados_bacen()$ts)[2]){
#           ts<-cbind(ts,xts(as.numeric(dados_bacen()$ts[,i]),as.Date(dados_bacen()$ts)))
#         }
#         colnames(ts)<-nomes
#       }
#       else if(is.null(dim(dados_bacen()$ts))){
#         ts<-xts(as.numeric(dados_bacen()$ts),as.Date(dados_bacen()$ts))
#       }
#       
#       ts<-na.omit(ts)
#       mes_final <- end(ts)[2]
#       ano_final <- end(ts)[1]
#       mes_inicial <- start(ts)[2]
#       ano_inicial <- start(ts)[1]
#       dygraph(ts, main ="Séries Bacen") %>%
#         #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#         dyOptions(connectSeparatedPoints = TRUE)%>%
#         dyRangeSelector(fillColor = "#2C65A1",
#                         dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                        paste0(ano_final,"-",mes_final,"-01")))
#     }
#   })
#   
#   # Mostrar legenda IGP
#   output$legenda_bacen<- renderTable({
#     tabela <- cbind(dados_bacen()$cod,dados_bacen()$nome)
#     colnames(tabela) <- c("Código", "Nome")
#     tabela
#   })
#     
#   
#   # Mostrar estatísticas descritivas
#   output$descritiva_bacen <- renderTable({
#     
#     n <- length(dados_bacen()$nome)
#     media <- 0
#     mediana <- 0
#     mini <- 0 
#     maxi <- 0
#     
#     if(n > 1){
#       for(i in 1:n){
#         media[i] <- mean(dados_bacen()$ts[,i], na.rm = T)
#         mediana[i] <- median(dados_bacen()$ts[,i], na.rm = T)
#         mini[i] <- min(dados_bacen()$ts[,i], na.rm = T)
#         maxi[i] <- max(dados_bacen()$ts[,i], na.rm = T)
#       }}else{
#         media <- mean(dados_bacen()$ts, na.rm = T)
#         mediana <- median(dados_bacen()$ts, na.rm = T)
#         mini <- min(dados_bacen()$ts, na.rm = T)
#         maxi <- max(dados_bacen()$ts, na.rm = T)
#       }
#     
#     tabela <- matrix(NA, ncol = 4, nrow = n)
#     colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#     rownames(tabela) <- dados_bacen()$nome
#     tabela[,1] <- media
#     tabela[,2] <- mediana
#     tabela[,3] <- mini
#     tabela[,4] <- maxi
#     tabela
#   })
#   
#   
#   
#   
#   
#  # BANCO ST: MUNDO ----------------------------------------------------------
#  # lista de séries disponíveis
#  output$tabela_mundo <- DT::renderDataTable({
#    datatable(cod_series_mundo[,c("COD","NOME","Periodicidade","Data.Inicial", "Data.Final","Fonte")], 
#              rownames = cod_series_mundo[,"COD"], 
#             #rownames =TRUE,
#              selection = 'single',
#              options = list(pageLength = 5))            
#  })
#  
#  # BOTÃO VISUALIZAR SÉRIES MUNDO
#  visu_mundo <- reactive({
#    # barrinha carregando
#    withProgress(message = 'Carregando:', value = 0, {
#      n <- 100
#      for (i in 1:n) {
#        incProgress(1/n, detail = paste0(i,"%"))
#        # Pause for 0.02 seconds to simulate a long computation.
#        Sys.sleep(0.005)
#      }
#    }) # fim barrinha carregando
#    
#    sel1 <- input$tabela_mundo_rows_selected
#    n <- length(sel1)
#    sel <- sel1[n]
#    nome <- cod_series_mundo$NOME[cod_series_mundo$COD == sel]
#    cod_nome <- cod_series_mundo$COD_NOME[cod_series_mundo$COD == sel]
#    src <- cod_series_mundo$SRC[cod_series_mundo$COD == sel]
#    coluna <- cod_series_mundo$COLUNA[cod_series_mundo$COD == sel]
#    dados <- getSymbols(cod_nome, src=src, auto.assign=FALSE, warnings = FALSE)[,coluna]
#    list(nome = nome, dados = dados)
#  })
#  
#  # Mostrar gráfico
#  output$grafico_stmundo <- renderDygraph({
#      dygraph(visu_mundo()$dados, main = visu_mundo()$nome) %>%
#        dyOptions(colors = "dodgerblue") %>%
#        dyRangeSelector(fillColor = "#2C65A1")
#  })
# 
# 
#  # Mostrar dados 
#  output$dados_stmundo <- renderTable({
#    data <- index(visu_mundo()$dados)
#    frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_mundo()$dados))
#    colnames(frame)[2] <- visu_mundo()$nome
#    frame
#  })
#  
#  # Mostrar estatísticas descritivas séries mundo
#  output$descritiva_stmundo <- renderTable({
# 
#    media <- 0
#    mediana <- 0
#    mini <- 0 
#    maxi <- 0
#    
#    media <- mean(visu_mundo()$dados, na.rm = T)
#    mediana <- median(visu_mundo()$dados, na.rm = T)
#    mini <- min(visu_mundo()$dados, na.rm = T)
#    maxi <- max(visu_mundo()$dados, na.rm = T)
# 
#    tabela <- matrix(NA, ncol = 4, nrow = 1)
#    colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#    rownames(tabela) <- visu_mundo()$nome
#    tabela[,1] <- media
#    tabela[,2] <- mediana
#    tabela[,3] <- mini
#    tabela[,4] <- maxi
#    tabela
#  })
# 
#  # MUND: EUA ----------------------------------------------------------------
#  # lista de séries disponíveis
#  mundo_eua <- reactive({
#    subset(cod_series_mundo, cod_series_mundo$LOCAL == "EUA")
#  })
#  
#  output$tabela_mundo_eua <- DT::renderDataTable({
#    datatable(mundo_eua()[,c("COD","NOME","Periodicidade","Fonte","Data.Final","Data.Inicial")], 
#              rownames = mundo_eua()[,"COD"], selection = 'single',
#              options = list(pageLength = 5))            
#  })
#  
#  # BOTÃO VISUALIZAR SÉRIES MUNDO EUA
#  visu_eua <- reactive({
#    # barrinha carregando
#    withProgress(message = 'Carregando:', value = 0, {
#      n <- 100
#      for (i in 1:n) {
#        incProgress(1/n, detail = paste0(i,"%"))
#        # Pause for 0.02 seconds to simulate a long computation.
#        Sys.sleep(0.005)
#      }
#    }) # fim barrinha carregando
#    
#    sel1 <- mundo_eua()[input$tabela_mundo_eua_rows_selected,1]
#    n <- length(sel1)
#    sel <- sel1[n]
#    nome <- mundo_eua()$NOME[mundo_eua()$COD == sel]
#    cod_nome <- mundo_eua()$COD_NOME[mundo_eua()$COD == sel]
#    src <- mundo_eua()$SRC[mundo_eua()$COD == sel]
#    coluna <- mundo_eua()$COLUNA[mundo_eua()$COD == sel]
#    dados <- getSymbols(cod_nome, src=src, auto.assign=FALSE, warnings = FALSE)[,coluna]
#    list(nome = nome, dados = dados)
#  })
#  
#  # Mostrar gráfico
#  output$grafico_stmundo_eua <- renderDygraph({
#    dygraph(visu_eua()$dados, main = visu_eua()$nome) %>%
#      dyOptions(colors = "dodgerblue") %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#  })
#  
#  # Mostrar dados 
#  output$dados_stmundo_eua <- renderTable({
#    data <- index(visu_eua()$dados)
#    frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_eua()$dados))
#    colnames(frame)[2] <- visu_eua()$nome
#    frame
#  })
#  
#  # Mostrar estatísticas descritivas séries mundo
#  output$descritiva_stmundo_eua <- renderTable({
#    
#    media <- 0
#    mediana <- 0
#    mini <- 0 
#    maxi <- 0
#    
#    media <- mean(visu_eua()$dados, na.rm = T)
#    mediana <- median(visu_eua()$dados, na.rm = T)
#    mini <- min(visu_eua()$dados, na.rm = T)
#    maxi <- max(visu_eua()$dados, na.rm = T)
#    
#    tabela <- matrix(NA, ncol = 4, nrow = 1)
#    colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#    rownames(tabela) <- visu_eua()$nome
#    tabela[,1] <- media
#    tabela[,2] <- mediana
#    tabela[,3] <- mini
#    tabela[,4] <- maxi
#    tabela
#  })
#  
#  ############ europa ###################################
#  
#  
#  # lista de séries disponíveis
#  mundo_europa <- reactive({
#    subset(cod_series_mundo, cod_series_mundo$LOCAL == "Europe")
#  })
#  
#  output$tabela_mundo_europa <- DT::renderDataTable({
#    datatable(mundo_europa()[,c("COD","NOME","Periodicidade","Fonte","Data.Final","Data.Inicial")], 
#              rownames = mundo_europa()[,"COD"], selection = 'single',
#              options = list(pageLength = 5))            
#  })
#  
#  output$linhaseuropa<-renderPrint({
#    
#    
#    sel1 <-  mundo_eua()[input$tabela_mundo_europa_rows_selected,1]
#    n <- length(sel1)
#    sel <- sel1[n]
#    nome <- as.character(mundo_europa()$NOME[mundo_europa()$COD == sel])
#    src <- mundo_europa()$SRC[mundo_europa()$COD == sel]
#    src
#    
#  })
#  
#  
#  # BOTÃO VISUALIZAR SÉRIES MUNDO EUA
#  visu_europa <- reactive({
#    # barrinha carregando
#    withProgress(message = 'Carregando:', value = 0, {
#      n <- 100
#      for (i in 1:n) {
#        incProgress(1/n, detail = paste0(i,"%"))
#        # Pause for 0.02 seconds to simulate a long computation.
#        Sys.sleep(0.005)
#      }
#    }) # fim barrinha carregando
#    
#    sel1 <- input$tabela_mundo_europa_rows_selected
#    n <- length(sel1)
#    sel <- sel1[n]
#    nome <- mundo_europa()$NOME[mundo_europa()$COD == sel]
#    cod_nome <- mundo_europa()$COD_NOME[mundo_europa()$COD == sel]
#    src <- mundo_europa()$SRC[mundo_europa()$COD == sel]
#    coluna <- mundo_europa()$COLUNA[mundo_europa()$COD == sel]
#    dados <- getSymbols(cod_nome, src=src, auto.assign=FALSE, warnings = FALSE)[,coluna]
#    list(nome = nome, dados = dados)
#  })
#  
#  # Mostrar gráfico
#  output$grafico_stmundo_europa <- renderDygraph({
#    dygraph(visu_europa()$dados, main = visu_europa()$nome) %>%
#      dyOptions(colors = "dodgerblue") %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#  })
#  
#  # Mostrar dados 
#  output$dados_stmundo_europa <- renderTable({
#    data <- index(visu_europa()$dados)
#    frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_europa()$dados))
#    colnames(frame)[2] <- visu_europa()$nome
#    frame
#  })
#  
#  # Mostrar estatísticas descritivas séries mundo
#  output$descritiva_stmundo_europa <- renderTable({
#    
#    media <- 0
#    mediana <- 0
#    mini <- 0 
#    maxi <- 0
#    
#    media <- mean(visu_europa()$dados, na.rm = T)
#    mediana <- median(visu_europa()$dados, na.rm = T)
#    mini <- min(visu_europa()$dados, na.rm = T)
#    maxi <- max(visu_europa()$dados, na.rm = T)
#    
#    tabela <- matrix(NA, ncol = 4, nrow = 1)
#    colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#    rownames(tabela) <- visu_europa()$nome
#    tabela[,1] <- media
#    tabela[,2] <- mediana
#    tabela[,3] <- mini
#    tabela[,4] <- maxi
#    tabela
#  })
#  
#  ############ asia ###################################
#  
#  
#  # lista de séries disponíveis
#  mundo_asia <- reactive({
#    subset(cod_series_mundo, cod_series_mundo$LOCAL == "Asia")
#  })
#  
#  output$tabela_mundo_asia <- DT::renderDataTable({
#    datatable(mundo_asia()[,c("COD","NOME","Periodicidade","Fonte","Data.Final","Data.Inicial")], 
#              rownames = mundo_asia()[,"COD"], selection = 'single',
#              options = list(pageLength = 5))            
#  })
#  
#  output$linhasasia<-renderPrint({
#    
#    
#    sel1 <-  mundo_eua()[input$tabela_mundo_asia_rows_selected,1]
#    n <- length(sel1)
#    sel <- sel1[n]
#    nome <- as.character(mundo_asia()$NOME[mundo_asia()$COD == sel])
#    src <- mundo_asia()$SRC[mundo_asia()$COD == sel]
#    src
#    
#  })
#  
#  
#  # BOTÃO VISUALIZAR SÉRIES MUNDO EUA
#  visu_asia <- reactive({
#    # barrinha carregando
#    withProgress(message = 'Carregando:', value = 0, {
#      n <- 100
#      for (i in 1:n) {
#        incProgress(1/n, detail = paste0(i,"%"))
#        # Pause for 0.02 seconds to simulate a long computation.
#        Sys.sleep(0.005)
#      }
#    }) # fim barrinha carregando
#    
#    sel1 <- input$tabela_mundo_asia_rows_selected
#    n <- length(sel1)
#    sel <- sel1[n]
#    nome <- mundo_asia()$NOME[mundo_asia()$COD == sel]
#    cod_nome <- mundo_asia()$COD_NOME[mundo_asia()$COD == sel]
#    src <- mundo_asia()$SRC[mundo_asia()$COD == sel]
#    coluna <- mundo_asia()$COLUNA[mundo_asia()$COD == sel]
#    dados <- getSymbols(cod_nome, src=src, auto.assign=FALSE, warnings = FALSE)[,coluna]
#    list(nome = nome, dados = dados)
#  })
#  
#  # Mostrar gráfico
#  output$grafico_stmundo_asia <- renderDygraph({
#    dygraph(visu_asia()$dados, main = visu_asia()$nome) %>%
#      dyOptions(colors = "dodgerblue") %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#  })
#  
#  # Mostrar dados 
#  output$dados_stmundo_asia <- renderTable({
#    data <- index(visu_asia()$dados)
#    frame <- data.frame(Data = as.character(data), dados = as.numeric(visu_asia()$dados))
#    colnames(frame)[2] <- visu_asia()$nome
#    frame
#  })
#  
#  # Mostrar estatísticas descritivas séries mundo
#  output$descritiva_stmundo_asia <- renderTable({
#    
#    media <- 0
#    mediana <- 0
#    mini <- 0 
#    maxi <- 0
#    
#    media <- mean(visu_asia()$dados, na.rm = T)
#    mediana <- median(visu_asia()$dados, na.rm = T)
#    mini <- min(visu_asia()$dados, na.rm = T)
#    maxi <- max(visu_asia()$dados, na.rm = T)
#    
#    tabela <- matrix(NA, ncol = 4, nrow = 1)
#    colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#    rownames(tabela) <- visu_asia()$nome
#    tabela[,1] <- media
#    tabela[,2] <- mediana
#    tabela[,3] <- mini
#    tabela[,4] <- maxi
#    tabela
#  })
#  
#  # Câmbio Dólar/Mundo --------------------------------------------------------
#  # baixar série
#  mundo_cambio <- reactive({
#    getSymbols(cambio_mundoInput(), src="yahoo", auto.assign=FALSE, warnings = FALSE)
#  })
#  output$cambio_teste <- renderPrint(mundo_cambio())
#  # verificar data de atualização
#  mundo_cambio_data <- reactive({
#    data <- strsplit(as.character(index(mundo_cambio()[dim(mundo_cambio())[1],])),split = "-" )[[1]]
#    data <- paste0(data[3],"/",data[2],"/",data[1])
#    data
#  })
#  # soltar última data de atualização
#  output$mundo_cambio_data <- renderText({mundo_cambio_data()})
#  # pegar valores da última data de atualização
#  output$mundo_cambio_Open <-  renderText(mundo_cambio()[dim(mundo_cambio())[1],paste0(cambio_mundoInput(),".Open")])
#  output$mundo_cambio_Close <- renderText(mundo_cambio()[dim(mundo_cambio())[1],paste0(cambio_mundoInput(),".Close")])
#  output$mundo_cambio_High <- renderText(mundo_cambio()[dim(mundo_cambio())[1],paste0(cambio_mundoInput(),".High")])
#  output$mundo_cambio_Low <- renderText(mundo_cambio()[dim(mundo_cambio())[1],paste0(cambio_mundoInput(),".Low")])
#  # soltar gráfico da série temporal
#  output$mundo_cambio_grafico <- renderDygraph({
#    dygraph(mundo_cambio()[,paste0(cambio_mundoInput(),".Close")], main = paste0("Câmbio Dólar/",input$mundo_cambio)) %>%
#      dyOptions(colors = '#2C65A1') %>% 
#      dyRangeSelector(fillColor = "#2C65A1")
#  })
#  
#  # MUNDO - INDICADORES SOCIAIS -------------------------------------------------------
#  
#  output$indic_pop <- renderGvis({
#  WorldPopulation=data.frame(Country=Population$Country,
#                             Population.in.millions=round(Population$Population/1e6,0),
#                             Rank=paste(Population$Country, "Rank:", Population$Rank))
#  colnames(WorldPopulation) <- c("País", "População em milhões", "Posição")
#  gvisGeoMap(WorldPopulation, "País", "População em milhões", "Posição",
#                   options=list(dataMode="regions", width=800, height=400))
# })
#  
#  output$indic_gini <- renderGvis({
#   gvisGeoMap(gini, "COD", "Gini",hovervar = "País",
#               options=list(dataMode="regions", width=800, height=400))
#  })
#  
#  output$indic_expec <- renderGvis({
#    gvisGeoMap(expec, "COD", "Expectativa", hovervar = "País",
#               options=list(dataMode="regions", width=800, height=400))
#  })
#  
#  output$indic_tmi <- renderGvis({
#    gvisGeoMap(tmi, "COD", "TMI", hovervar = "País",
#               options=list(dataMode="regions", width=800, height=400))
#  })
#  
#  output$indic_pop <- renderGvis({
#    gvisGeoMap(pop, "COD", "População", hovervar = "País",
#               options=list(dataMode="regions", width=800, height=400))
#    
#    
#  })
#  
#  
#  #######################montando o favoritos ################################################
#  
#  #######favoritos IGP -########################
#  
#  
#  favorito_igp <- reactive({
#    if(input$add_igp == 0){ return("")
#    }else{
#      isolate({
#        # linhas selecionadas
#        #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # códigos selecionados
#        codigos <- as.numeric(table_igp()[input$tabela_igp_rows_selected,1]) #_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <- table_igp()[table_igp()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="IGP"
#        tabela$periodicidade="M"
#        tabela$Inicio<-table_igp()[table_igp()[,"Código"] %in% codigos, "Início"]
#        tabela$Fim<-table_igp()[table_igp()[,"Código"] %in% codigos, "Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  ############favoritos IBGE #######################################
#  favorito_IBGE <- reactive({
#    if(input$add_IBGE== 0){ return("")
#    }else{
#      isolate({
#        #linhas <-as.character(input$tabela_IBGE_rows_selected)) # retorna linhas selecionadas da tabela_igp
#        # tabela=data.frame(base_IBGE[base_IBGE,base_IBGE$Cód. %in% input$tabela_IBGE_rows_selected,c(1,2)])
#        linhas<-base_IBGE[input$tabela_IBGE_rows_selected,1]
#        tabela=subset(base_IBGE,base_IBGE$Cód. %in% linhas )[,c(1,2)]
#        tabela[,1]<- as.character(tabela[,1])
#        #tabela=data.frame("Código"=linhas,"Nome"="Anna")
#        tabela$fonte="IBGE"
#        tabela$periodicidade=subset(base_IBGE,base_IBGE$Cód. %in% linhas)[,4]
#        tabela$Inicio=subset(base_IBGE,base_IBGE$Cód. %in% linhas )[,5]
#        tabela$Fim=subset(base_IBGE,base_IBGE$Cód. %in% linhas)[,6]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  
#  ########## favoritos BACEN####################
#  
#  favorito_bacen<-reactive({
#    linhas<-base_bacen[input$bacentabela_rows_selected,1]
#    tabela=subset(base_bacen,base_bacen$Cód. %in% linhas )[,c(1,2)]
#    tabela[,1]<- as.character(tabela[,1])
#    #tabela=data.frame("Código"=linhas,"Nome"="Anna")
#    tabela$fonte="BACEN"
#    tabela$periodicidade=subset(base_bacen,base_bacen$Cód. %in% linhas )[,4]
#    tabela$Inicio=subset(base_bacen,base_bacen$Cód. %in% linhas )[,5]
#    tabela$Fim=subset(base_bacen,base_bacen$Cód. %in% linhas )[,6]
#    names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#    tabela
#  })
#  
#  
#  ################# favoritos mundo ####################################
#  
#  favorito_mundo <- reactive({
#    if(input$add_mundo== 0){ return("")
#    }else{
#      isolate({
#        isolate(linhas <-cod_series_mundo[input$tabela_mundo_rows_selected,1]) # retorna linhas selecionadas da tabela_igp
#        tabela=data.frame(cod_series_mundo[cod_series_mundo[,1] %in% linhas,c(1,2)])
#        tabela[,1]<- as.character(tabela[,1])
#        #tabela=data.frame("Código"=linhas,"Nome"="Anna")
#        tabela$fonte="Mundo"
#        tabela$Periodicidade<-cod_series_mundo[cod_series_mundo[,1] %in% linhas,6]
#        tabela$Inicio<-""
#        tabela$Fim<-""
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  
#  
#  ################# favoritos EUA ####################################
#  
#  favorito_eua <- reactive({
#    if(input$add_mundo_eua== 0){ return("")
#    }else{
#      isolate({
#        isolate(linhas <-cod_series_mundo[input$tabela_mundo_eua_rows_selected,1]) # retorna linhas selecionadas da tabela_igp
#        tabela=data.frame(cod_series_mundo[cod_series_mundo[,1] %in% linhas,c(1,2)])
#        tabela[,1]<- as.character(tabela[,1])
#        #tabela=data.frame("Código"=linhas,"Nome"="Anna")
#        tabela$fonte="EUA"
#        tabela$Periodicidade<-cod_series_mundo[cod_series_mundo[,1] %in% linhas,6]
#        tabela$Inicio<-""
#        tabela$Fim<-""
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  
#  ################# favoritos America LAtina ####################################
#  
#  favorito_america_latina <- reactive({
#    if(input$add_mundo_al== 0){ return("")
#    }else{
#      isolate({
#        isolate(linhas <-cod_series_mundo[input$tabela_mundo_al_rows_selected,1]) # retorna linhas selecionadas da tabela_igp
#        tabela=data.frame(cod_series_mundo[cod_series_mundo[,1] %in% linhas,c(1,2)])
#        tabela[,1]<- as.character(tabela[,1])
#        #tabela=data.frame("Código"=linhas,"Nome"="Anna")
#        tabela$fonte="America"
#        tabela$Periodicidade<-cod_series_mundo[cod_series_mundo[,1] %in% linhas,6]
#        tabela$Inicio<-""
#        tabela$Fim<-""
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  ################# favoritos Europa ####################################
#  
#  favorito_europa <- reactive({
#    if(input$add_mundo_europa== 0){ return("")
#    }else{
#      isolate({
#        isolate(linhas <-cod_series_mundo[input$tabela_mundo_europa_rows_selected,1]) # retorna linhas selecionadas da tabela_igp
#        tabela=data.frame(cod_series_mundo[cod_series_mundo[,1] %in% linhas,c(1,2)])
#        tabela[,1]<- as.character(tabela[,1])
#        #tabela=data.frame("Código"=linhas,"Nome"="Anna")
#        tabela$fonte="Europa"
#        tabela$Periodicidade<-cod_series_mundo[cod_series_mundo[,1] %in% linhas,6]
#        tabela$Inicio<-""
#        tabela$Fim<-""
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  
#  ################# favoritos asia ####################################
#  
#  favorito_asia <- reactive({
#    if(input$add_mundo_asia== 0){ return("")
#    }else{
#      isolate({
#        isolate(linhas <-cod_series_mundo[input$tabela_mundo_asia_rows_selected,1]) # retorna linhas selecionadas da tabela_igp
#        tabela=data.frame(cod_series_mundo[cod_series_mundo[,1] %in% linhas,c(1,2)])
#        tabela[,1]<- as.character(tabela[,1])
#        #tabela=data.frame("Código"=linhas,"Nome"="Anna")
#        tabela$fonte="Asia"
#        tabela$Periodicidade<-cod_series_mundo[cod_series_mundo[,1] %in% linhas,6]
#        tabela$Inicio<-""
#        tabela$Fim<-""
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  
#  #######favoritos ipc -########################
#  favorito_ipc <- reactive({
#    if(input$add_ipc == 0){ return("")
#    }else{
#        # linhas selecionadas
#        isolate(linhas <-subset_ipc[input$tabela_ipc_rows_selected,1]) # retorna linhas selecionadas da tabela_igp
#        # códigos selecionados
#        tabela<-subset_ipc[subset_ipc$Código %in% linhas ,c("Código","Nome")]
#        tabela$fonte="IPC"
#        tabela$periodicidade="M"
#        tabela$Inicio<-subset_ipc[input$tabela_ipc_rows_selected, "Início"]
#        tabela$Fim<-subset_ipc[input$tabela_ipc_rows_selected, "Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#         tabela}
#  })
#  
#  
#  
#  ##################### favoritos IPA ########################
#  #############################Favoritos IPA10###########################
#  
#  favorito_IPA10 <- reactive({
#    if(input$add_ipa10 == 0){ return("")
#    }else{
#      isolate({
#        # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_ipa10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_ipa10()[filtro_ipa10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        #
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="IPA10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos <- as.numeric(filtro_ipa10()[input$filtro_ipa10_rows_selected,1]) #_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <- filtro_ipa10()[filtro_ipa10()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
# 
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="IPA10"
#        tabela$periodicidade="M"
#        tabela$Inicio<-filtro_ipa10()[filtro_ipa10()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-filtro_ipa10()[filtro_ipa10()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  
#  #############################Favoritos IPADI###########################
#  
#  favorito_IPADI <- reactive({
#    if(input$add_ipadi == 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_ipadi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_ipadi()[filtro_ipadi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="IPADI"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_ipadi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_ipadi()[filtro_ipadi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        #
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="IPAdi"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos <- filtro_ipadi()[input$filtro_ipadi_rows_selected,1] #_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <- filtro_ipadi()[filtro_ipadi()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="IPADI"
#        tabela$periodicidade="M"
#        tabela$Inicio<-filtro_ipadi()[filtro_ipadi()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-filtro_ipadi()[filtro_ipadi()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  
#  #############################Favoritos IPAM###########################
#  
#  favorito_IPAM <- reactive({
#    if(input$add_ipam == 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_ipam_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_ipam()[filtro_ipam()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="IPAM"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_ipa10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_ipa10()[filtro_ipa10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        #
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="IPA10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos <- filtro_ipam()[input$filtro_ipam_rows_selected,1] #_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <- filtro_ipam()[filtro_ipam()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="IPAM"
#        tabela$periodicidade="M"
#        tabela$Inicio<-filtro_ipam()[filtro_ipam()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-filtro_ipam()[filtro_ipam()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  
#  
#  ##################### favoritos INCC########################
#  #############################Favoritos INCC10###########################
#  
#  favorito_INCC10 <- reactive({
#    if(input$add_incc10 == 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos <-  filtro_incc10()[input$filtro_incc10_rows_selected,1] #_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="INCC10"
#        tabela$periodicidade="M"
#        tabela$Inicio<-filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  
#  #############################Favoritos INCCM###########################
#  
#  favorito_INCCM <- reactive({
#    if(input$add_inccm == 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos <-filtro_inccm()[input$filtro_inccm_rows_selected,1] #_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <- filtro_inccm()[filtro_inccm()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="INCCM"
#        tabela$periodicidade="M"
#        tabela$Inicio<-filtro_inccm()[filtro_inccm()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-filtro_inccm()[filtro_inccm()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  # 
#  
#  #############################Favoritos INCCM###########################
#  
#  favorito_INCCDI <- reactive({
#    if(input$add_inccdi == 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_inccdi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCCDI"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos <-filtro_inccdi()[input$filtro_inccdi_rows_selected,1] #_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="INCCDI"
#        tabela$periodicidade="M"
#        tabela$Inicio<-filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
# 
#      })}
#  })
#  ################ favoritos sondagem da industria #########################
#  favorito_industria <- reactive({
#    if(input$add_industria== 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_inccdi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCCDI"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos<-filtro_inccdi()[input$tabela_industria_rows_selected,1]#_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <-table_industria()[table_industria()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="Industria"
#        tabela$periodicidade="M"
#        tabela$Inicio<-table_industria()[table_industria()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-table_industria()[table_industria()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  ############## sondagem comercio ################################################
#  
#  ################ favoritos sondagem da comercio #########################
#  favorito_comercio <- reactive({
#    if(input$add_comercio== 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_inccdi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCCDI"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos<-filtro_inccdi()[input$tabela_comercio_rows_selected,1]#_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <-table_comercio()[table_comercio()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="Comercio"
#        tabela$periodicidade="M"
#        tabela$Inicio<-table_comercio()[table_comercio()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-table_comercio()[table_comercio()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  ############## sondagem Servico ################################################
#  
#  ################ favoritos sondagem da Servico #########################
#  favorito_servico <- reactive({
#    if(input$add_servico== 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_inccdi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCCDI"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos<-filtro_inccdi()[input$tabela_servico_rows_selected,1]#_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <-table_servico()[table_servico()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="Servico"
#        tabela$periodicidade="M"
#        tabela$Inicio<-table_servico()[table_servico()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-table_servico()[table_servico()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  
#  ############## sondagem Construção ################################################
#  
#  ################ favoritos sondagem da Construção #########################
#  favorito_construcao <- reactive({
#    if(input$add_construcao== 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_inccdi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCCDI"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos<-filtro_inccdi()[input$tabela_construcao_rows_selected,1]#_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <-table_construcao()[table_construcao()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="Servico"
#        tabela$periodicidade="M"
#        tabela$Inicio<-table_construcao()[table_construcao()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-table_construcao()[table_construcao()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  
#  ############## sondagem consumidor ################################################
#  
#  ################ favoritos sondagem da consumidor #########################
#  favorito_consumidor <- reactive({
#    if(input$add_consumidor== 0){ return("")
#    }else{
#      isolate({
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_inccdi_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_inccdi()[filtro_inccdi()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCCDI"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        # # linhas selecionadas
#        # #isolate(linhas <- input$tabela_igp_rows_selected) # retorna linhas selecionadas da tabela_igp
#        # # códigos selecionados
#        # codigos <- input$filtro_incc10_rows_selected #_Selected: retorna as linhas selecionadas da tabela_completa
#        # codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # # códigos selecionados
#        # tabela <- filtro_incc10()[filtro_incc10()[,"Código"] %in% codigos, c("Código","Nome")]
#        # #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        # 
#        # #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        # tabela[,1]<- as.character(tabela[,1])
#        # tabela$fonte="INCC10"
#        # names(tabela)<-c("Código","Nome","Fonte")
#        # tabela
#        codigos<-filtro_inccdi()[input$tabela_consumidor_rows_selected,1]#_Selected: retorna as linhas selecionadas da tabela_completa
#        #codigos <- gsub(" ", "", codigos, fixed = TRUE)
#        # códigos selecionados
#        tabela <-table_consumidor()[table_consumidor()[,"Código"] %in% codigos, c("Código","Nome")]
#        #data.frame("Código"=linhas,"Nome"=table_igp()[3,2])
#        
#        #tabela=data.frame(table_igp()[linhas,c("Código","Nome")])
#        #tabela[,1]<- as.character(tabela[,1])
#        tabela$fonte="Consumidor"
#        tabela$periodicidade="M"
#        tabela$Inicio<-table_consumidor()[table_consumidor()[,"Código"] %in% codigos,"Início"]
#        tabela$Fim<-table_consumidor()[table_consumidor()[,"Código"] %in% codigos,"Fim"]
#        names(tabela)<-c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")
#        tabela
#        
#      })}
#  })
#  
#  ########### montando o carrinho coms os favoritos ########################################
#  
#  favoritos_completo <- reactiveValues()
#  favoritos_completo$df <- data.frame("Código" = character(0),"Nome"= character(0),"Fonte"=character(0),"Periodicidade"=character(0),"Inicio"=character(0),"Fim"=character(0))
#  
#  
#  novas_linhas <- observe({
#    
#    if(exists("save_favoritos")==TRUE){
#      isolate(favoritos_completo$df <- rbind(favoritos_completo$df,save_favoritos))}
#    if(input$add_igp != 0){
#      novalinha_igp <- isolate(favorito_igp())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_igp)))}
#    if(input$add_IBGE != 0){
#      novalinha_ibge <- isolate(favorito_IBGE())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_ibge)))}
#    if(input$add_bacen != 0){
#      novalinha_bacen <- isolate(favorito_bacen())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_bacen)))}
#    if(input$add_mundo != 0){
#      novalinha_mundo <- isolate(favorito_mundo())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_mundo)))}
#    if(input$add_mundo_eua != 0){
#      novalinha_eua <- isolate(favorito_eua())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_eua)))}
#    if(input$add_mundo_al != 0){
#      novalinha_america_latina <- isolate(favorito_america_latina())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_america_latina)))}
#    if(input$add_mundo_europa != 0){
#      novalinha_europa <- isolate(favorito_europa())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_europa)))}
#    if(input$add_mundo_asia != 0){
#      novalinha_asia <- isolate(favorito_asia())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_asia)))}
#    if(input$add_ipa10 != 0){
#      novalinha_ipa10 <- isolate(favorito_IPA10())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_ipa10)))}
#    if(input$add_ipadi != 0){
#      novalinha_ipadi <- isolate(favorito_IPADI())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_ipadi)))}
#    if(input$add_ipam != 0){
#      novalinha_ipam <- isolate(favorito_IPAM())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_ipam)))}
#    if(input$add_ipc != 0){
#      novalinha_ipc <- isolate(favorito_ipc())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_ipc)))}
#    if(input$add_incc10 != 0){
#      novalinha_incc10 <- isolate(favorito_INCC10())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_incc10)))}
#    if(input$add_inccm != 0){
#      novalinha_inccm <- isolate(favorito_INCCM())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_inccm)))}
#    if(input$add_inccdi != 0){
#      novalinha_inccdi <- isolate(favorito_INCCDI())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_inccdi)))}
#    if(input$add_industria!= 0){
#      novalinha_industria <- isolate(favorito_industria())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_industria)))}
#    if(input$add_comercio!= 0){
#      novalinha_comercio <- isolate(favorito_comercio())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_comercio)))}
#    if(input$add_servico!= 0){
#      novalinha_servico <- isolate(favorito_servico())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_servico)))}
#    if(input$add_construcao!= 0){
#      novalinha_construcao <- isolate(favorito_construcao())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_construcao)))}
#    if(input$add_consumidor!= 0){
#      novalinha_consumidor <- isolate(favorito_consumidor())
#      isolate(favoritos_completo$df <- unique(rbind(favoritos_completo$df,novalinha_consumidor)))}
#  })
#  
#  ##### Salvando Favoritos ##############
#  
# #  output$download_favoritos<- downloadHandler(
# #    
# #    filename = "save_favoritos.csv",
# #    content = function(file) {
# #      frame <- favoritos_completo$df
# #      write.csv2(frame, file, row.names = F)
# #    
# #    }
# #    
# #  )
#  
#  observeEvent(input$Save,{
#    
#    saveData <- function(data){
#      if (exists("responses")){
#        responses <- rbind(responses, data)
#      } else {
#        responses <- data
#      }
# 
#      write.csv2(responses,"./data/save_favoritos.csv",row.names = FALSE)
#    }
#    
#    saveData(favoritos_completo$df)
#  })
# 
#  
#  #############montando a tabela com os favoritos #######################################################
#  
# #  output$favoritos<- DT::renderDataTable({
# #    
# #    if(is.null(save_favoritos)) {
# #      
# #      
# #      datatable(favoritos_completo$df, 
# #                #rownames = favoritos_completo$df[,"Código"],
# #                options = list(lengthMenu = c(5, 10, 20),
# #                               pageLength = 10,
# #                               searching = FALSE,paging = FALSE))
# #    }else{
# #        
# #      datatable(save_favoritos, 
# #                #rownames = favoritos_completo$df[,"Código"],
# #                options = list(lengthMenu = c(5, 10, 20),
# #                               pageLength = 10,
# #                               searching = FALSE,paging = FALSE))
# #      }
# #    
# #             
# #    })
#  
#  output$favoritos<- DT::renderDataTable(
#      datatable(favoritos_completo$df,
#                rownames = favoritos_completo$df[,"Código"],
#                options = list(lengthMenu = c(5, 10, 20),
#                               pageLength = 10,
#                               searching = FALSE,paging = FALSE))
#  )
#  
# 
#  
#  
#  ####################### montando a tabela com os favoritos ####################################
#   st_favoritos<- reactive({
#    periodiciodade<-favoritos_completo$df[input$favoritos_rows_selected,"Periodicidade"]
#    
#    if(length(unique(periodiciodade))==1){
#      linhas <- as.character(favoritos_completo$df[input$favoritos_rows_selected,1]) #_Selected: retorna as linhas selecionadas da tabela_completa
#      #linhas <- gsub(" ", "", linhas, fixed = TRUE)
#      # códigos selecionados
#      #favoritas <- favoritos_completo$df[favoritos_completo$df[,1] %in% as.character(input$favoritos_rows_selected), c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")]#favoritas <- favoritos_completo$df[favoritos_completo$df[,"Código"] %in% linhas
#      favoritas <- favoritos_completo$df[input$favoritos_rows_selected,c("Código","Nome","Fonte","Periodicidade","Inicio","Fim")]
#      #favoritas=data.frame(favoritos_completo$df[input$favoritos_rows_selected,c("Código","Nome","Fonte")])
#      
#      masc_IBGE<-favoritas[,3]== "IBGE"
#      masc_IGP<-favoritas[,3]== "IGP"
#      masc_IPC<-favoritas[,3]== "IPC"
#      masc_Mundo<-favoritas[,3]== "Mundo"
#     masc_IPA10<-favoritas[,3]== "IPA10"
#      masc_IPADI<-favoritas[,3]== "IPADI"
#      masc_IPAM<-favoritas[,3]== "IPAM"
#      masc_INCC10<-favoritas[,3]== "INCC10"
#      masc_INCCM<-favoritas[,3]== "INCCM"
#      masc_INCCDI<-favoritas[,3]== "INCCDI"
#      masc_bacen<-favoritas[,3]== "BACEN"
#      masc_industria<-favoritas[,3]== "Industria"
#      masc_comercio<-favoritas[,3]== "Comercio"
#      masc_servico<-favoritas[,3]== "Servico"
#      masc_construcao<-favoritas[,3]== "Construcao"
#      masc_consumidor<-favoritas[,3]== "Consumidor"
#      masc_eua<-favoritas[,3]== "EUA"
#      masc_america<-favoritas[,3]== "America"
#      masc_europa<-favoritas[,3]== "Europa"
#      masc_asia<-favoritas[,3]== "Asia"
#      
#      igp<-favoritas[masc_IGP,]
#      ipc<-favoritas[masc_IPC,]
#     ibge<-favoritas[masc_IBGE,]
#     mundo<-favoritas[masc_Mundo,]
#      ipa10<-favoritas[masc_IPA10,]
#      ipadi<-favoritas[masc_IPADI,]
#      ipam<-favoritas[masc_IPAM]
#      incc10<-favoritas[masc_INCC10,]
#      inccm<-favoritas[masc_INCCM,]
#      inccdi<-favoritas[masc_INCCDI,]
#      bacen<-favoritas[masc_bacen,]
#      industria<-favoritas[masc_industria,]
#      comercio<-favoritas[masc_comercio,]
#      servico<-favoritas[masc_servico,]
#      construcao<-favoritas[masc_construcao,]
#      consumidor<-favoritas[masc_consumidor,]
#      eua<-favoritas[masc_eua,]
#      america<-favoritas[masc_america,]
#      europa<-favoritas[masc_europa,]
#      asia<-favoritas[masc_asia,]
#      
#      
#      
#      libera=0
#    }else if(length(unique(periodiciodade))>=1){
#      libera=1
#      igp<-NULL
#      ibge<-NULL
#      mundo<-NULL
#      ipa10<-NULL
#      ipadi<-NULL
#      ipam<-NULL
#      incc10<-NULL
#      inccm<-NULL
#      inccdi<-NULL
#      bacen<-NULL
#      industria<-NULL
#      comercio<-NULL
#      servico<-NULL
#      construcao<-NULL
#      consumidor<-NULL
#      eua<-NULL
#      america<-NULL
#      europa=NULL
#      asia<-NULL
#      ipc<-NULL
#      }
#     
#    
#    #list("IGP"=igp,"IBGE"=ibge,"Mundo"=mundo,"Favoritas"=linhas,"IPA10"=ipa10,"IPADI"=ipadi,"IPAM"=ipam,"INCC10"=incc10,"INCCM"=inccm,"INCCDI"=inccdi,"BACEN"=bacen)
#    
#    list("IGP"=igp,"IPC"=ipc,"BACEN"=bacen,"IBGE"=ibge,"libera"=libera,"periodicidade"=periodiciodade,"IPA10"=ipa10,"IPADI"=ipadi,"IPAM"=ipam,"INCC10"=incc10,"INCCM"=inccm,"INCCDI"=inccdi,"Mundo"=mundo,"Industria"=industria,"Comercio"=comercio,"Servico"=servico,"Construcao"=construcao,"Consumidor"=consumidor,"EUA"=eua,"America"=america,"Europa"=europa,"Asia"=asia)
#  })
#  
#  output$libera <- reactive({
#    
#    if(st_favoritos()$libera== 0){condicao = 0}else{condicao = 1}
#    condicao
#    
#  })
#  outputOptions(output, 'libera', suspendWhenHidden=FALSE)
#  
# 
#  
#  output$linhas<-renderPrint({
#    favoritos_completa()$ts
#    })
#  
#  output$testeperiodicidade<-renderTable({
#    data.frame("casa")
#  })
#  
#  ########### MONTANDO O FAVORITOS IGP ###################
# 
#  favoritos_igp<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$IGP[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # #list(ts =ts, nome = st_favoritos()$IGP[,"Nome"], codigo = codigos_info,df=df,data=data)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$IGP)){
#      codigos<-as.character(st_favoritos()$IGP[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.character(as.Date(fgv_ts))
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL
#    }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
#  })
# 
#  ########### MONTANDO O FAVORITOS IBGE ###################
# 
#  favoritos_IBGE<-reactive({
# 
#    if(!is.null(st_favoritos()$IBGE)){
#      linhas<-as.numeric(as.character(st_favoritos()$IBGE[,1]))
#      periodicidade=subset(base_IBGE,base_IBGE$Cód. %in% linhas )[1,4]
#      base<-subset(base_IBGE,base_IBGE$Cód. %in% linhas)
#      ts=BETS.get(linhas,periodicidade = periodicidade)[[1]]
#      if(!is.null(dim(ts))){
#        colnames(ts)<-subset(base_IBGE,base_IBGE$Cód. %in% as.numeric(as.character(st_favoritos()$IBGE[,1])))[,2]
#      }
#      data=as.character(BETS.get(linhas,periodicidade = periodicidade)[[2]])
#      nome=subset(base_IBGE,base_IBGE$Cód. %in% as.numeric(as.character(st_favoritos()$IBGE[,1])))[,2]
#      cod=linhas
#    }else{
#      ts=NULL
#      data=NULL
#      nome=NULL
#      cod=NULL
#    }
# 
#    list(ts=ts,data=data,nome=nome,cod=cod)
#  })
# 
#  ########## favoritos bacen ##################################
# 
#  favoritos_BACEN<-reactive({
# 
#    # linhas<-which(dados[,1]==as.character(st_favoritos()$IBGE[1,1]))
#    # for (i in 2:length(st_favoritos()$IBGE[,1])){
#    #  linhas<-c(linhas,which(dados[,1]==as.character(st_favoritos()$IBGE[i,1])))
#    # }
#    # api_ibge(linhas)
#    #  api_ibge(linhas)
# 
#    if(!is.null(st_favoritos()$BACEN)){
#      linhas<-as.numeric(as.character(st_favoritos()$BACEN[,1]))
#      periodicidade=subset(base_bacen,base_bacen$Cód. %in% linhas )[1,4]
#      base<-subset(base_bacen,base_bacen$Cód. %in% linhas )
#      ts=BETS.get(linhas,periodicidade = periodicidade)[[1]]
#      if(!is.null(dim(ts))){
#        colnames(ts)<-subset(base_bacen,base_bacen$Cód. %in% as.numeric(as.character(st_favoritos()$BACEN[,1])))[,2]
#      }
#      data=as.character(BETS.get(linhas,periodicidade = periodicidade)[[2]])
#      nome=subset(base_bacen,base_bacen$Cód. %in% as.numeric(as.character(st_favoritos()$BACEN[,1])))[,2]
#      cod=linhas
#    }else{
#      ts=NULL
#      data=NULL
#      nome=NULL
#      cod=NULL
#    }
# 
#    list(ts=ts,data=data,nome=nome,cod=cod)
#  })
# 
#   ###########favoritos mundo ################################
# 
#  favoritos_mundo<-reactive({
#    if(!is.null(st_favoritos()$Mundo)){
#      codigos<-as.character(st_favoritos()$Mundo[,1])
# 
#      tabela_mundo<-subset(cod_series_mundo,cod_series_mundo$COD %in% codigos)
#      n <- dim(st_favoritos()$Mundo)[1]
#      nome <- as.character(tabela_mundo$NOME)
#      cod_nome <- as.character(tabela_mundo$COD_NOME)
#      src <- as.character(tabela_mundo$SRC)
#      coluna <- as.character(tabela_mundo$COLUNA)
# 
#      for (i in 1:n){
#        if (i<=1){
#          ts<-getSymbols(cod_nome[1], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[1]]
#          #        nome<-colnames(x_ts)
#          #        ts<-ts((x_ts))}
#        }else{
#          ts<-cbind(ts,getSymbols(cod_nome[i], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[i]])
#          #        ts<-cbind(ts,ts(x_ts))
#          #        nome<-c(nome,colnames(x_ts))}}
#        }}
#     
#      data=index(ts)
#    }else{
#      ts=NULL
#      nome=NULL
#      data=NULL
# 
#    }
# 
# 
# 
#       list(ts=ts,nome = nome,data=data)
# 
#  })
# 
# 
# 
#  ###########favoritos eua ################################
# 
#  favoritos_eua<-reactive({
#    if(!is.null(st_favoritos()$EUA)){
#      codigos<-as.character(st_favoritos()$EUA[,1])
# 
#      tabela_mundo<-subset(cod_series_mundo,cod_series_mundo$COD %in% codigos)
#      n <- dim(st_favoritos()$Mundo)[1]
#      nome <- as.character(tabela_mundo$NOME)
#      cod_nome <- as.character(tabela_mundo$COD_NOME)
#      src <- as.character(tabela_mundo$SRC)
#      coluna <- as.character(tabela_mundo$COLUNA)
# 
#      for (i in 1:n){
#        if (i<=1){
#          ts<-getSymbols(cod_nome[1], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[1]]
#          #        nome<-colnames(x_ts)
#          #        ts<-ts((x_ts))}
#        }else{
#          ts<-cbind(ts,getSymbols(cod_nome[i], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[i]])
#          #        ts<-cbind(ts,ts(x_ts))
#          #        nome<-c(nome,colnames(x_ts))}}
#        }}
#      data=index(ts)
#    }else{
#      ts=NULL
#      nome=NULL
#      data=NULL
# 
#    }
# 
# 
#    list(ts=ts,nome = nome,data=data)
# 
#  })
# 
#  ###########favoritos America ################################
# 
#  favoritos_america<-reactive({
#    if(!is.null(st_favoritos()$America)){
#      codigos<-as.character(st_favoritos()$America[,1])
# 
#      tabela_mundo<-subset(cod_series_mundo,cod_series_mundo$COD %in% codigos)
#      n <- dim(st_favoritos()$Mundo)[1]
#      nome <- as.character(tabela_mundo$NOME)
#      cod_nome <- as.character(tabela_mundo$COD_NOME)
#      src <- as.character(tabela_mundo$SRC)
#      coluna <- as.character(tabela_mundo$COLUNA)
# 
#      for (i in 1:n){
#        if (i<=1){
#          ts<-getSymbols(cod_nome[1], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[1]]
#          #        nome<-colnames(x_ts)
#          #        ts<-ts((x_ts))}
#        }else{
#          ts<-cbind(ts,getSymbols(cod_nome[i], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[i]])
#          #        ts<-cbind(ts,ts(x_ts))
#          #        nome<-c(nome,colnames(x_ts))}}
#        }}
#      data=as.character(index(ts))
#      #ts <- as.ts(ts, start = as.character(start(ts)),as.character(end = end(ts)))
#    }else{
#      ts=NULL
#      nome=NULL
#      data=NULL
# 
#    }
# 
# 
#    list(ts=ts,nome = nome,data=data)
# 
#  })
# 
#  ###########favoritos Europa ################################
# 
#  favoritos_europa<-reactive({
#    if(!is.null(st_favoritos()$Europa)){
#      codigos<-as.character(st_favoritos()$Europa[,1])
# 
#      tabela_mundo<-subset(cod_series_mundo,cod_series_mundo$COD %in% codigos)
#      n <- dim(st_favoritos()$Mundo)[1]
#      nome <- as.character(tabela_mundo$NOME)
#      cod_nome <- as.character(tabela_mundo$COD_NOME)
#      src <- as.character(tabela_mundo$SRC)
#      coluna <- as.character(tabela_mundo$COLUNA)
# 
#      for (i in 1:n){
#        if (i<=1){
#          ts<-getSymbols(cod_nome[1], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[1]]
#          #        nome<-colnames(x_ts)
#          #        ts<-ts((x_ts))}
#        }else{
#          ts<-cbind(ts,getSymbols(cod_nome[i], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[i]])
#          #        ts<-cbind(ts,ts(x_ts))
#          #        nome<-c(nome,colnames(x_ts))}}
#        }}
#      data=index(ts)
#    }else{
#      ts=NULL
#      nome=NULL
#      data=NULL
# 
#    }
# 
# 
#    list(ts=ts,nome = nome,data=data)
# 
#  })
# 
#  ###########favoritos Asia ################################
# 
#  favoritos_asia<-reactive({
#    if(!is.null(st_favoritos()$Asia)){
#      codigos<-as.character(st_favoritos()$Asia[,1])
# 
#      tabela_mundo<-subset(cod_series_mundo,cod_series_mundo$COD %in% codigos)
#      n <- dim(st_favoritos()$Mundo)[1]
#      nome <- as.character(tabela_mundo$NOME)
#      cod_nome <- as.character(tabela_mundo$COD_NOME)
#      src <- as.character(tabela_mundo$SRC)
#      coluna <- as.character(tabela_mundo$COLUNA)
# 
#      for (i in 1:n){
#        if (i<=1){
#          ts<-getSymbols(cod_nome[1], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[1]]
#          #        nome<-colnames(x_ts)
#          #        ts<-ts((x_ts))}
#        }else{
#          ts<-cbind(ts,getSymbols(cod_nome[i], src=src, auto.assign=FALSE, warnings = FALSE)[,coluna[i]])
#          #        ts<-cbind(ts,ts(x_ts))
#          #        nome<-c(nome,colnames(x_ts))}}
#        }}
#      data=index(ts)
#    }else{
#      ts=NULL
#      nome=NULL
#      data=NULL
# 
#    }
# 
# 
#    list(ts=ts,nome = nome,data=data)
# 
#  })
# 
# 
#  ########################Favoritos IPA###################
#  ##############favoritosIPA10#######################
# 
#  favoritos_ipa10<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$IPA10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#    if(!is.null(st_favoritos()$IPA10)){   codigos<-as.character(st_favoritos()$IPA10[,1])
#    #codigos<-input$favoritos_rows_selected
#    palavras<-as.character(codigos)
#    paste(palavras, collapse = "' '")
#    strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#    dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#    FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#    odbcCloseAll()
# 
#    ######## indo recortar a base ################################
#    base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#    start_ano=as.numeric(substr(base[1,1],1,4))
#    start_mes=as.numeric(substr(base[1,1],5,6))
# 
#    fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#    if(length(codigos)>1){
#      for (i in 2:length(codigos)){
#        base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#        start_ano=as.numeric(substr(base[1,1],1,4))
#        start_mes=as.numeric(substr(base[1,1],5,6))
#        fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#      colnames(fgv_ts)<-paste0("ST_",codigos)
#    }
#    #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#    codigos_info <- paste0("ST_",codigos)
#    data<-as.character(as.Date(fgv_ts))
#    df<-cbind(data.frame(data),data.frame(fgv_ts))
#    ts=fgv_ts
#    nome= codigos_info
#    codigo = codigos_info
#    }else{
#        ts=NULL
#        nome=NULL
#        codigo=NULL
#        df=NULL
#        data=NULL
#   }
# 
#  list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
# 
#  ##############favoritosIPADI#######################
# 
#  favoritos_ipadi<- reactive({
# #
# #    codigos<-as.character(st_favoritos()$IPADI[,1])
# #
# #    codigos_info <- paste0("ST_",codigos)
# #    ts = FGV[,codigos_info]
# #    df=data.frame(ts)
# #    Data<-as.character(as.Date(ts))
# #    df<-cbind(data.frame(Data),df)
# #    list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    # codigos<-as.character(st_favoritos()$IPA10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#   if(!is.null(st_favoritos()$IPADI)){
#   codigos<-as.character(st_favoritos()$IPADI[,1])
#    #codigos<-input$favoritos_rows_selected
#    palavras<-as.character(codigos)
#    paste(palavras, collapse = "' '")
#    strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#    dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#    FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#    odbcCloseAll()
# 
#    ######## indo recortar a base ################################
#    base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#    start_ano=as.numeric(substr(base[1,1],1,4))
#    start_mes=as.numeric(substr(base[1,1],5,6))
# 
#    fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#    if(length(codigos)>1){
#      for (i in 2:length(codigos)){
#        base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#        start_ano=as.numeric(substr(base[1,1],1,4))
#        start_mes=as.numeric(substr(base[1,1],5,6))
#        fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#      colnames(fgv_ts)<-paste0("ST_",codigos)
#    }
#    #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#    codigos_info <- paste0("ST_",codigos)
#    data<-as.character(as.Date(fgv_ts))
#    df<-cbind(data.frame(Date),data.frame(fgv_ts))
#    ts=fgv_ts
#    nome= codigos_info
#    codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL
#    }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
# 
#  })
# 
#  ##############favoritosIPADI#######################
# 
#  favoritos_ipam<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$IPAM[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$IPAM)){
#      codigos<-as.character(st_favoritos()$IPAM[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      Data<-as.character(as.Date(fgv_ts))
#      df<-cbind(data.frame(Data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL
#    }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
# 
# 
# 
#  ########################Favoritos INCC###################
#  ##############favoritos INCC10#######################
# 
#  favoritos_incc10<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$INCC10)){
#      codigos<-as.character(st_favoritos()$INCC10[,1])
#     #codigos<-input$favoritos_rows_selected
#     palavras<-as.character(codigos)
#     paste(palavras, collapse = "' '")
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
# 
#    ######## indo recortar a base ################################
#    base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#    start_ano=as.numeric(substr(base[1,1],1,4))
#    start_mes=as.numeric(substr(base[1,1],5,6))
# 
#    fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#    if(length(codigos)>1){
#      for (i in 2:length(codigos)){
#        base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#        start_ano=as.numeric(substr(base[1,1],1,4))
#        start_mes=as.numeric(substr(base[1,1],5,6))
#        fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#      colnames(fgv_ts)<-paste0("ST_",codigos)
#    }
#    #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     data<-as.Date(fgv_ts)
#     df<-cbind(data.frame(data),data.frame(fgv_ts))
#     ts=fgv_ts
#     nome= codigos_info
#     codigo = codigos_info
#     }else{
#       ts=NULL
#       nome=NULL
#       codigo=NULL
#       df=NULL
#       data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
#  #
# 
#  output$dados_incc<-renderTable({
# 
#    data.frame(as.character(as.Date(favoritos_incc10()$ts)))
# 
#  })
# 
# 
#  ##############favoritos INCCM#######################
# 
#  favoritos_inccm<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$INCCM)){
#      codigos<-as.character(st_favoritos()$INCCM[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.Date(fgv_ts)
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
#  })
# 
# 
#  ##############favoritos INCCDI#######################
#  #
#  favoritos_inccdi<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCCDI[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$INCCDI)){
#      codigos<-as.character(st_favoritos()$INCCDI[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.Date(fgv_ts)
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
# 
# 
# ###############favoritos IPC ################
# 
# favoritos_ipc<-reactive({
#   if(!is.null(st_favoritos()$IPC)){
#     codigos<-as.character(st_favoritos()$IPC[,1])
#     #codigos<-input$favoritos_rows_selected
#     palavras<-as.character(codigos)
#     paste(palavras, collapse = "' '")
#     strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#     dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#     FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#     odbcCloseAll()
# 
#     ######## indo recortar a base ################################
#     base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#     start_ano=as.numeric(substr(base[1,1],1,4))
#     start_mes=as.numeric(substr(base[1,1],5,6))
# 
#     fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#     if(length(codigos)>1){
#       for (i in 2:length(codigos)){
#         base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#         start_ano=as.numeric(substr(base[1,1],1,4))
#         start_mes=as.numeric(substr(base[1,1],5,6))
#         fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#       colnames(fgv_ts)<-paste0("ST_",codigos)
#     }
#     #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#     codigos_info <- paste0("ST_",codigos)
#     data<-as.character(as.Date(fgv_ts))
#     df<-cbind(data.frame(data),data.frame(fgv_ts))
#     ts=fgv_ts
#     nome= codigos_info
#     codigo = codigos_info
#   }else{
#     ts=NULL
#     nome=NULL
#     codigo=NULL
#     df=NULL
#     data=NULL
#   }
# 
#   list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
# })
# 
# 
# 
# 
# 
#  ################# favoritos industria ############################
#  #
#  favoritos_industria<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCCDI[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$Industria)){
#      codigos<-as.character(st_favoritos()$Industria[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.Date(fgv_ts)
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
# 
# 
#  ##############favoritos Comercio#######################
#  #
#  favoritos_comercio<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCCDI[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$Comercio)){
#      codigos<-as.character(st_favoritos()$Comercio[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.Date(fgv_ts)
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
# 
#  ##############favoritos Comercio#######################
#  #
#  favoritos_servico<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCCDI[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$Servico)){
#      codigos<-as.character(st_favoritos()$Servico[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.Date(fgv_ts)
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
# 
#  ##############favoritos construcao#######################
#  #
#  favoritos_construcao<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCCDI[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$Construcao)){
#      codigos<-as.character(st_favoritos()$Construcao[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.Date(fgv_ts)
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
# 
#  ##############favoritos consumidor#######################
#  #
#  favoritos_consumidor<- reactive({
# 
#    # codigos<-as.character(st_favoritos()$INCCDI[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
# 
#    # codigos<-as.character(st_favoritos()$INCC10[,1])
#    #
#    # codigos_info <- paste0("ST_",codigos)
#    # ts = FGV[,codigos_info]
#    # df=data.frame(ts)
#    # Data<-as.character(as.Date(ts))
#    # df<-cbind(data.frame(Data),df)
#    # list(ts =ts, nome = codigos_info, codigo = codigos_info,df=df,data=data)
#    if(!is.null(st_favoritos()$Consumidor)){
#      codigos<-as.character(st_favoritos()$Consumidor[,1])
#      #codigos<-input$favoritos_rows_selected
#      palavras<-as.character(codigos)
#      paste(palavras, collapse = "' '")
#      strng_usada<-paste0("'",paste(palavras, collapse = ","),"'")
#      dbhandle <- odbcDriverConnect('driver={SQL Server};server=DC7025;database=BI_INDECO;trusted_connection=true')
#      FGV_base<-sqlQuery(dbhandle,paste0("exec IEC_PI_CONSULTA_R_NOVA_V2 '20020100','20150600', ",strng_usada))
#      odbcCloseAll()
# 
#      ######## indo recortar a base ################################
#      base<-FGV_base[FGV_base$CD_SERIE==codigos[1],]
#      start_ano=as.numeric(substr(base[1,1],1,4))
#      start_mes=as.numeric(substr(base[1,1],5,6))
# 
#      fgv_ts<-ts(base$Valor,start=c(start_ano,start_mes),freq=12)
# 
#      if(length(codigos)>1){
#        for (i in 2:length(codigos)){
#          base<-FGV_base[FGV_base$CD_SERIE==codigos[i],]
#          start_ano=as.numeric(substr(base[1,1],1,4))
#          start_mes=as.numeric(substr(base[1,1],5,6))
#          fgv_ts<-cbind(fgv_ts,ts(base$Valor,start=c(start_ano,start_mes),freq=12))}
# 
#        colnames(fgv_ts)<-paste0("ST_",codigos)
#      }
#      #tabela <- table_igp()[input$tabela_igp_rows_selected, c("Código","Nome")]
#      codigos_info <- paste0("ST_",codigos)
#      data<-as.Date(fgv_ts)
#      df<-cbind(data.frame(data),data.frame(fgv_ts))
#      ts=fgv_ts
#      nome= codigos_info
#      codigo = codigos_info
#    }else{
#      ts=NULL
#      nome=NULL
#      codigo=NULL
#      df=NULL
#      data=NULL }
# 
#    list(ts=ts,nome= nome,codigo = codigo,df=df,data=data)
# 
#  })
#  #
#  #
#  # output$dados_incc<-renderTable({
#  #
#  #   data.frame(as.character(as.Date(favoritos_inccm()$ts)))
#  #
#  # })
# 
#   ################# montando a ts dos favoritos ################################################
# 
#  favoritos_ts <- reactiveValues()
# 
#  novas_colunas <- observe({
#    if("IBGE" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$IBGE <- isolate(favoritos_IBGE()$ts)
#      favoritos_ts$NOMEIBGE <- isolate(favoritos_IBGE()$nome)
#      favoritos_ts$DATAIBGE <- isolate(favoritos_IBGE()$data)}
#    if("BACEN" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$BACEN <- isolate(favoritos_BACEN()$ts)
#      favoritos_ts$NOMEBACEN <- isolate(favoritos_BACEN()$nome)
#      favoritos_ts$DATABACEN <- isolate(favoritos_BACEN()$data)}
#    if("IGP" %in% as.character(favoritos_completo$df[input$favoritos_rows_selected,3])){
#      favoritos_ts$IGP <- isolate(favoritos_igp()$ts)
#      favoritos_ts$NOMEIGP <- isolate(favoritos_igp()$nome)
#      favoritos_ts$DATAIGP <- isolate(favoritos_igp()$data)}
#    if("IPC" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$IPC <- isolate(favoritos_ipc()$ts)
#      favoritos_ts$NOMEIPC <- isolate(favoritos_ipc()$nome)
#      favoritos_ts$DATAIPC <- isolate(favoritos_ipc()$data)}
#    if("Mundo" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Mundo <- isolate(favoritos_mundo()$ts)
#      favoritos_ts$NOMEMundo <- isolate(favoritos_mundo()$nome)
#      favoritos_ts$DATAMundo <- isolate(favoritos_mundo()$data)}
#    if("IPA10" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$IPA10 <- isolate(favoritos_ipa10()$ts)
#      favoritos_ts$NOMEIPA10 <- isolate(favoritos_ipa10()$nome)
#      favoritos_ts$DATAIPA10 <- isolate(favoritos_ipa10()$data)}
#    if("IPADI" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$IPADI <-isolate(favoritos_ipadi()$ts)
#      favoritos_ts$NOMEIPADI <- isolate(favoritos_ipadi()$nome)
#      favoritos_ts$DATAIPADI <- isolate(favoritos_ipadi()$data)}
#    if("IPAM" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$IPAM <- isolate(favoritos_ipam()$ts)
#      favoritos_ts$NOMEIPAM <- isolate(favoritos_ipam()$nome)
#      favoritos_ts$DATAIPAM <- isolate(favoritos_ipam()$data)}
#    if("INCC10" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$INCC10 <- isolate(favoritos_incc10()$ts)
#      favoritos_ts$NOMEINCC10 <- isolate(favoritos_incc10()$nome)
#      favoritos_ts$DATAINCC10 <- isolate(as.Date(favoritos_incc10()$ts))}
#    if("INCCM" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$INCCM <- isolate(favoritos_inccm()$ts)
#      favoritos_ts$NOMEINCCM <- isolate(favoritos_inccm()$nome)
#      favoritos_ts$DATAINCCM <- isolate(favoritos_inccm()$data)}
#    if("INCCDI" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$INCCDI <- isolate(favoritos_inccdi()$ts)
#      favoritos_ts$NOMEINCCDI <- isolate(favoritos_inccdi()$nome)
#      favoritos_ts$DATAINCCDI <- isolate(favoritos_inccdi()$data)}
#    if("Industria" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Industria <- isolate(favoritos_industria()$ts)
#      favoritos_ts$NOMEIndustria <- isolate(favoritos_industria()$nome)
#      favoritos_ts$DATAIndustria <- isolate(favoritos_industria()$data)}
#    if("Comercio" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Comercio <- isolate(favoritos_comercio()$ts)
#      favoritos_ts$NOMEComercio <- isolate(favoritos_comercio()$nome)
#      favoritos_ts$DATAComercio <- isolate(favoritos_comercio()$data)}
#    if("Servico" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Servico <- isolate(favoritos_servico()$ts)
#      favoritos_ts$NOMEServico <- isolate(favoritos_servico()$nome)
#      favoritos_ts$DATAServico <- isolate(favoritos_servico()$data)}
#    if("Construcao" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Construcao <- isolate(favoritos_construcao()$ts)
#      favoritos_ts$NOMEConstrucao <- isolate(favoritos_construcao()$nome)
#      favoritos_ts$DATAConstrucao <- isolate(favoritos_construcao()$data)}
#    if("Consumidor" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Consumidor <- isolate(favoritos_consumidor()$ts)
#      favoritos_ts$NOMEConsumidor<- isolate(favoritos_consumidor()$nome)
#      favoritos_ts$DATAConsumidor <- isolate(favoritos_consumidor()$data)}
#    if("EUA" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$EUA <- isolate(favoritos_eua()$ts)
#      favoritos_ts$NOMEEUA<- isolate(favoritos_eua()$nome)
#      favoritos_ts$DATAEUA <- isolate(favoritos_eua()$data)}
#    if("Europa" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Europa <- isolate(favoritos_europa()$ts)
#      favoritos_ts$NOMEEuropa<- isolate(favoritos_europa()$nome)
#      favoritos_ts$DATAEuropa <- isolate(favoritos_europa()$data)}
#    if("America" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$America <- isolate(favoritos_america()$ts)
#      favoritos_ts$NOMEAmerica<- isolate(favoritos_america()$nome)
#      favoritos_ts$DATAAmerica<- isolate(favoritos_america()$data)}
#    if("Asia" %in% favoritos_completo$df[input$favoritos_rows_selected,3]){
#      favoritos_ts$Asia <- isolate(favoritos_asia()$ts)
#      favoritos_ts$NOMEAsia<- isolate(favoritos_asia()$nome)
#      favoritos_ts$DATAAsia<- isolate(favoritos_asia()$data)}
#  })
# 
#  favoritos_completa<-reactive({
#    
#    withProgress(message = 'Carregando:', value = 0, {
#      n <- 100
#      for (i in 1:n) {
#        incProgress(1/n, detail = paste0(i,"%"))
#        # Pause for 0.02 seconds to simulate a long computation.
#        Sys.sleep(0.2)
#      }
#    }) 
# 
#    fontes<-unique(favoritos_completo$df[input$favoritos_rows_selected,3])
# 
#    problema=FALSE
# 
#    if(length(fontes)==1){
#      ts<-favoritos_ts[[as.character(fontes[1])]]
#      nome<-favoritos_ts[[paste0("NOME",as.character(fontes[1]))]]
#      d<-favoritos_ts[[paste0("DATA",as.character(fontes[1]))]]
# 
#    }else{
#      ts<-favoritos_ts[[as.character(fontes[1])]]
#      nome<-favoritos_ts[[paste0("NOME",as.character(fontes[1]))]]
#      d<-favoritos_ts[[paste0("DATA",as.character(fontes[1]))]]
# 
# 
#      for(i in 2: length(fontes)){
#        if(frequency(favoritos_ts[[as.character(fontes[i])]]) == frequency(ts)){
# 
#          ts<-cbind(ts,favoritos_ts[[as.character(fontes[i])]])
#          nome<-c(nome,favoritos_ts[[paste0("NOME",as.character(fontes[i]))]])
#          d<-unique(cbind(d,favoritos_ts[[paste0("DATA",as.character(fontes[i]))]]))
# 
#        }else{
# 
#          problema<-TRUE
# 
#        }}
# 
# 
#      if (!is.null(dim(ts))){
#        colnames(ts)<-nome
#      }
# 
#    }
# 
#    #     df<-data.frame(d)
#    #     df2<-data.frame(ts)
#    #     df2$Data<-df
# 
# 
# 
#    #validate(try(problema!=TRUE),"Escolha séries com a mesma periodicidade")
# 
#    list(ts=ts,data=d,nome=nome,problema=problema,periodicidade=unique(favoritos_completo$df[input$favoritos_rows_selected,4]),fontes=unique(favoritos_completo$df[input$favoritos_rows_selected,4]))
#    #list(ts=ts,nome=nome,problema=problema)
#  })
# 
#  #######################fazendo output da periodicidade #####################################
#  output$problema<- renderText({
#    favoritos_completa()$problema
#  })
#  ################### Mostrando os dados do favoritos #####################################
#   output$dados_favoritos<- renderTable({
#     
#     # barrinha carregando
#     
#     Data<-data.frame("Data"=favoritos_completa()$data)
#     ts<-data.frame(favoritos_completa()$ts)
#     names(ts)<-favoritos_completa()$nome
#     
#     #     names(ts)<-favoritos_completa()$nome
#     #     $ts$Periodo<-Data
#     
#     #ts<-ts[,c(2,1)]
#     
#     Data<-cbind(Data,ts)
#     if (ncol(Data)<5){
#       Data
#     }else{
#       Data[,c(1:5)]
#     }
#     
#     Data
# 
#     # data.frame(favoritos_mundo()$nome)
# 
#     #data.frame(favoritos_completo$df[favoritos_completo$df[,1] %in% as.numeric(input$favoritos_rows_selected),3])
# 
#     #data.frame(as.character(input$favoritos_rows_selected))
# 
#     # fontes<-unique(favoritos_completo$df[favoritos_completo$df[1,] %in% as.character(input$favoritos_rows_selected),3])
#     # data.frame(favoritos_ts[[as.character(fontes[1])]])
# 
#     # (data.frame(favoritos_completo$df[favoritos_completo$df[1,] %in% as.character(input$favoritos_rows_selected),3]))
#    })
# 
#  ########## estatistica descritiva favoritos ########################
#  
#  # Mostrar estatísticas descritivas
#  output$descritiva_favoritos <- renderTable({
#    
#    n <- length(favoritos_completa()$nome)
#    media <- 0
#    mediana <- 0
#    mini <- 0 
#    maxi <- 0
#    
#    if(n > 1){
#      for(i in 1:n){
#        media[i] <- mean(favoritos_completa()$ts[,i], na.rm = T)
#        mediana[i] <- median(favoritos_completa()$ts[,i], na.rm = T)
#        mini[i] <- min(favoritos_completa()$ts[,i], na.rm = T)
#        maxi[i] <- max(favoritos_completa()$ts[,i], na.rm = T)
#      }}else{
#        media <- mean(favoritos_completa()$ts, na.rm = T)
#        mediana <- median(favoritos_completa()$ts, na.rm = T)
#        mini <- min(favoritos_completa()$ts, na.rm = T)
#        maxi <- max(favoritos_completa()$ts, na.rm = T)
#      }
#    
#    tabela <- matrix(NA, ncol = 4, nrow = n)
#    colnames(tabela) <- c("Média","Mediana","Mínimo","Máximo")
#    rownames(tabela) <- favoritos_completa()$nome
#    tabela[,1] <- media
#    tabela[,2] <- mediana
#    tabela[,3] <- mini
#    tabela[,4] <- maxi
#    tabela
#  })
#  
#  
#  
#  
#  #############mostrando os gráfico dos favoritos ####################################
# 
#  output$grafico_favoritos <- renderDygraph({
# 
# 
#    # # Encontrar n
#    # n <- length(favoritos_completa()$ts)
#    # # Fazer o gráfico
#    # if(n == 1){
#    #   dygraph(favoritos_completa()$ts, main = "") %>%
#    #     dySeries("V1", label =  favoritos_completa()$nome) %>%
#    #     dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#    #     dyRangeSelector(fillColor = "#2C65A1")
#    # }else{
#    #   dygraph(favoritos_completa()$ts, main = "") %>%
#    #     dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#    #     dyRangeSelector(fillColor = "#2C65A1")
#    # }
# 
#    if(favoritos_completa()$periodicidade=="Diária"){
#      dygraph(favoritos_completa()$ts, main = "")%>%
#        dyOptions(colors = "dodgerblue") %>%
#        dyRangeSelector(fillColor = "#2C65A1")
#    }
# 
#    else if(favoritos_completa()$periodicidade=="M"||favoritos_completa()$periodicidade=="TRI"){
#      mes_final <- end(favoritos_completa()$ts)[2]
#      ano_final <- end(favoritos_completa()$ts)[1]
#      mes_inicial <- start(favoritos_completa()$ts)[2]
#      ano_inicial <- start(favoritos_completa()$ts)[1]
# 
#      #Encontrar n
#      n <- length(favoritos_completa()$ts)
#      # Fazer o gráfico
#      if(n == 1){
#        dygraph(favoritos_completa()$ts, main = "") %>%
#          dySeries("V1", label = favoritos_completa()$nome, strokeWidth = 2) %>%
#          dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#          dyRangeSelector()
#      }else{
#        dygraph(favoritos_completa()$ts, main = "") %>%
#          dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1"), strokeWidth = 2) %>%
#          dyRangeSelector()}}
# 
#    else if(favoritos_completa()$periodicidade=="A"){
# 
#      if(!is.null(dim(favoritos_completa()$ts))){
#        ts<-xts(as.numeric(favoritos_completa()$ts[,1]),as.Date(favoritos_completa()$ts))
#        #nomes<-colnames(completo()$ts)
#        nomes<-favoritos_completa()$nome
#        for(i in 2:dim(favoritos_completa()$ts)[2]){
#          ts<-cbind(ts,xts(as.numeric(favoritos_completa()$ts[,i]),as.Date(favoritos_completa()$ts)))
#        }
#        colnames(ts)<-nomes
#      }
#      else if(is.null(dim(favoritos_completa()$ts))){
#        ts<-xts(as.numeric(favoritos_completa()$ts),as.Date(favoritos_completa()$ts))
#      }
# 
#      ts<-na.omit(ts)
#      mes_final <- end(ts)[2]
#      ano_final <- end(ts)[1]
#      mes_inicial <- start(ts)[2]
#      ano_inicial <- start(ts)[1]
#      dygraph(ts, main ="Suas Séries Favoritas") %>%
#        #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#        dyOptions(connectSeparatedPoints = TRUE)%>%
#        dyRangeSelector(fillColor = "#2C65A1",
#                        dateWindow = c(paste0(ano_inicial,"-",mes_inicial,"-01"),
#                                       paste0(ano_final,"-",mes_final,"-01")))
#    }
#  })
# 
#  
#  ################graficos boletim macro
#  
#  output$falencia <- renderGvis({
#    gvisColumnChart(falencia, xvar ="Valor" ,yvar ="Porcentagem", options = list(title="Percentual de Empresas da Amostra que Iriam à Falência (%)",legend="none"))
#  })
#  
#  output$falencia2 <- renderGvis({
#    gvisColumnChart(falencia2, xvar ="Valor" ,yvar ="Porcentagem", options = list(title="Percentual de Empresas da Amostra que Iriam à Falência (%)",legend="none"))
#  })
#  
#  ############## tabela monitor pib##################
#  
#  output$monitor_pib<- DT::renderDataTable(
#    datatable(monitor_pib, 
#              rownames = FALSE,
#              options = list(pageLength = 50,
#                             searching = FALSE,dom = 't',autoWidth = TRUE,
#                             columnDefs = list(width = '50px',targets =3)))
#  )
#  
#  
#  ###############GRAFICO sONDAGENS###################################
#  
#  
#  output$sondagens<- renderDygraph({
#    # Encontrar n
#    n <- length(tsondagem)
#    dygraph(tsondagem, main = "Índices de Confiança FGV/IBRE") %>%
#      #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#      #dySeries("V1", label = names_s) %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#    
#  })
#  
#  output$sondagens_servico<- renderDygraph({
#    # Encontrar n
#    n <- length(tsondagem)
#    dygraph(tsondagem[,1], main = "Serviço") %>%
#      #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#      #dySeries("V1", label = names_s) %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#    
#  })
#  
#  output$sondagens_industria<- renderDygraph({
#    # Encontrar n
#    n <- length(tsondagem)
#    dygraph(tsondagem[,2], main = "Indústria") %>%
#      #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#      #dySeries("V1", label = names_s) %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#    
#  })
#  
#  output$sondagens_comercio<- renderDygraph({
#    # Encontrar n
#    n <- length(tsondagem)
#    dygraph(tsondagem[,3], main = "Comércio") %>%
#      #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#      #dySeries("V1", label = names_s) %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#    
#  })
#  
#  output$sondagens_construcao<- renderDygraph({
#    # Encontrar n
#    n <- length(tsondagem)
#    dygraph(tsondagem[,4], main = "Construção") %>%
#      #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#      #dySeries("V1", label = names_s) %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#    
#  })
#  
#  
#  output$sondagens_consumidor<- renderDygraph({
#    # Encontrar n
#    n <- length(tsondagem)
#    dygraph(tsondagem[,5], main = "Consumidor") %>%
#      #dyOptions(colors = RColorBrewer::brewer.pal(n, "Set1")) %>%
#      #dySeries("V1", label = names_s) %>%
#      dyRangeSelector(fillColor = "#2C65A1")
#  })
#  
#  
#  
#  #  output$monitor_pib<- renderTable({
#  #    
#  #    data.frame(monitor_pib)
#  #    
#  #  })
#  #  
#  
#  
#  
#  
#  
# #  output$monitor_pib<- renderTable({
# #    
# #    data.frame(monitor_pib)
# #    
# #  })
# #  
#  
 
 
})