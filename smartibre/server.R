shinyServer(function(input, output,session){
  
  
  # PÁGINA INICIAL -------------------------------------------------------------------------
  
  observeEvent(input$action_pesquisar,{
    updateTabItems(session, "menu_esquerda", "bd_pesquisar")
  })
  
  observeEvent(input$action_favoritos,{
    updateTabItems(session, "menu_esquerda", "bd_favoritos")
  })
  
  observeEvent(input$action_relatorios,{
    updateTabItems(session, "menu_esquerda", "bd_relatorios")
  })
  
  observeEvent(input$action_parametrico,{
    updateTabItems(session, "menu_esquerda", "mod_param")
  })
  
  # NOTIFICAÇÕES PARA O USUÁRIO -------------------------------------------------------------
  
  output$notif_user <- renderMenu({
    
    mensagens <- data.frame(text = c("Blog do IBRE - Nova atualização"),
                            status = c("primary"),
                            icon = c("comments-o"),
                            href = c("http://blogdoibre.fgv.br/"))
    
    msgs <- apply(mensagens, 1, function(row) {
      notificationItem(text = row[["text"]], status = row[["status"]], icon = icon(row[["icon"]]), href = row[["href"]])
    })
    
    # saída da função
    dropdownMenu(type = "notifications", .list = msgs)
  })
  
  
  # MENU PESQUISAR -----------------------------------------------------------------------------
  
  # > aba 'Busca' ----------------------------------------
  
  # buscar
  BETS_search <- reactiveValues()
  observeEvent(input$action_search_code, {
    BETS_search$data <- tryCatch(BETS.search(code = input$search_code, view = F), error = function(e) NULL)
  })
  observeEvent(input$action_search_description, {
    BETS_search$data <- tryCatch(BETS.search(description = input$search_description, view = F, lang = "pt"), error = function(e) NULL) 
  })
  observeEvent(input$action_search_src, {
    BETS_search$data <- tryCatch(BETS.search(src = input$search_src, view = F, lang = "pt"), error = function(e) NULL)
  })
  
  # tabela resultante ao fazer a busca
  output$BETS_search <- renderDataTable({
    if(is.null(BETS_search$data)){NULL
    }else{
      x <- BETS_search$data
      colnames(x) <- c("Cód.", "Descrição", "Unid.", "Period.", "Início", "Fim", "Fonte")
      x}
  },  options = list(pageLength = 5, searching = T))
  
  # > texto 'busca' ------------------
  
  output$texto_busca <- renderUI({
    if(is.null(BETS_search$data)){
      span("Use o painel à esquerda para buscar séries.", style = "color:grey")
    }else{
      ""
    }
  })
  
  # > aba 'Consultar' ----------------------------------------
  
  # códigos selecionados na aba busca
  busca_codigos_selecionados <- reactive({BETS_search$data[input$BETS_search_rows_selected,"code"]})
  
  # adicionar códigos na aba consultar
  consultar_codigos <- reactiveValues()
  observeEvent(input$action_add_consulta, {
    consultar_codigos$data <- unique(c(consultar_codigos$data, busca_codigos_selecionados()))
  })
  
  # criar tabela para a aba consultar
  output$tabela_consultar <- renderDataTable({
    if(is.null(consultar_codigos$data)){ NULL
    }else{
      codigos <- consultar_codigos$data
      data <- data.frame(t(sapply(as.numeric(codigos), FUN = function(x){BETS.search(code = x, view = F)})))
      rownames(data) <- 1:nrow(data)
      colnames(data) <- c("Cód.", "Descrição", "Unid.", "Period.", "Início", "Fim", "Fonte")
      data
    }
  },  options = list(pageLength = 5, searching = T))
  
  # remover códigos selecionados na aba consultar
  observeEvent(input$action_remove_consulta, {
    codigos <- consultar_codigos$data
    data <- data.frame(t(sapply(as.numeric(codigos), FUN = function(x){BETS.search(code = x, view = F)})))
    consultar_codigos$data <- unlist(data[-c(input$tabela_consultar_rows_selected),"code"])
  })
  
  # remover todos os códigos na aba consultar
  observeEvent(input$action_removeall_consulta, {
    consultar_codigos$data <- NULL
  })
  
  # texto consultar
  output$texto_consultar <- renderUI({
    if(is.null(consultar_codigos$data)){
      span("Nenhuma série adicionada à lista de consulta.", style = "color:grey")
    }else{
      ""
    }
  })
  
  # >> visualizar séries temporais da aba 'consultar' -------------------
  
  # baixar séries selecionadas da aba 'Consultar'
  series_consultar <- reactive({
    selecionados <- consultar_codigos$data[input$tabela_consultar_rows_selected]
    periodo <- unique(unlist(data.frame(t(sapply(as.numeric(selecionados), FUN = function(x){BETS.search(code = x, view = F)})))$periodicity))
    if(length(periodo) > 1){
      list(nomes = NULL, df = NULL, st = NULL)
    }else{
      if(length(selecionados) == 1){
        st <- BETS.get(selecionados, data.frame = T)
        st <- xts(st[,2], st[,1])
        names(st) <- selecionados
        frame <- data.frame(data = as.character(index(st)), st)
        rownames(frame) <- 1:nrow(frame)
        colnames(frame) <- c("data", selecionados)
        list(nomes = selecionados, st = st, df = frame)
      }else{
        baixar.list <- lapply(selecionados, FUN = BETS.get, data.frame = T)
        names(baixar.list) <- selecionados
        st <- do.call(cbind, lapply(baixar.list, FUN = function(x) xts(x[,2],x[,1])))
        names(st) <- selecionados
        frame <- data.frame(data = as.character(index(st)), st)
        rownames(frame) <- 1:nrow(frame)
        colnames(frame) <- c("data", selecionados)
        list(nomes = selecionados, st = st, df = frame)
      }
    }
  })
  
  # habilitar botões de visualizar séries se (não) houver algo selecionado na aba consulta
  observe({
    toggleState(id = "download_series_consultar", condition = !(length(consultar_codigos$data[input$tabela_consultar_rows_selected]) == 0 | length(unique(unlist(data.frame(t(sapply(as.numeric(consultar_codigos$data[input$tabela_consultar_rows_selected]),
                                                                                                                                                                                   FUN = function(x){BETS.search(code = x, view = F)})))$periodicity))) > 1))
    toggleState(id = "action_ver_consultar", condition = length(consultar_codigos$data[input$tabela_consultar_rows_selected]) != 0)
  })
  
  # tabela (consultar)
  output$ver_tabela_consultar <- renderTable({
    tail(series_consultar()$df, n = 12)
  })
  
  # gráfico (consultar)
  grafico_consultar <- reactive({
    if(is.null(series_consultar()$st)){ 
      NULL
    }else{
      dygraph(series_consultar()$st) %>%
        dyOptions(colors = brewer.pal(9, "Set1")) %>%
        dyRangeSelector(fillColor = "#F7F7F7") %>%
        dyLegend(labelsDiv = "legenda_grafico_consultar", show = "always")
    }
  })
  
  output$grafico_consultar <- renderDygraph({
    grafico_consultar()
  })
  
  # > visualizar gráfico e tabela
  output$ver_consultar <- renderUI({
    if(is.null(series_consultar()$st)){
      span("Séries selecionadas com periodicidade diferente. Selecione apenas séries de mesma periodicidade.", style = "color:grey")
    }else{
      tabsetPanel(
        tabPanel("Gráfico",
                 fluidRow(
                   column(10, br(), dygraphOutput("grafico_consultar")),
                   column(2, br(), textOutput("legenda_grafico_consultar"))
                 ),
                 hr(),
                 downloadButton("download_grafico_consultar", 'Exportar Gráfico')
        ),
        tabPanel("Dados",
                 br(),
                 span("Veja as 12 observações mais recentes:", style = "color:grey"), br(), br(),
                 tableOutput("ver_tabela_consultar")
        )
      )
    }
  })
  
  observeEvent(input$action_ver_consultar, {
    showModal(
      modalDialog(
        uiOutput("ver_consultar"),
        title = div("Visualização de Séries Temporais", style = "font-weight:bold"),
        easyClose = TRUE, footer = modalButton("Fechar"), size = "l"
      )
    )
  })
  
  # baixar gráfico.html
  output$download_grafico_consultar <- downloadHandler(
    filename = "SMARTIBRE_grafico.html",
    content = function(file) saveWidget(grafico_consultar(), file, selfcontained = T)
  )
  
  output$download_series_consultar <- downloadHandler(
    filename = "SMARTIBRE_series.csv",
    content = function(file) write.csv2(series_consultar()$df, file, row.names = F, na = "")
  )
  
  
  # > ativar botões de adicionar/remover/remover tudo/favoritos ------------------
  observe({
    # se estiver na aba 'Busca' e selecionou alguma linha do resultado da busca: habilitar botão adicionar e favoritos
    if(input$tab_pesquisar == "Busca" & length(busca_codigos_selecionados()) != 0){
      updateButton(session, "action_add_consulta", disabled = F, style = "primary")
      updateButton(session, "action_remove_consulta", disabled = T, style = "default")
      updateButton(session, "action_removeall_consulta", disabled = T, style = "default")
      updateButton(session, "action_add_favoritos", disabled = F, style = "danger")
    }else if(input$tab_pesquisar == "Busca" & length(busca_codigos_selecionados()) == 0){
      updateButton(session, "action_add_consulta", disabled = T, style = "default")
      updateButton(session, "action_remove_consulta", disabled = T, style = "default")
      updateButton(session, "action_removeall_consulta", disabled = T, style = "default")
      updateButton(session, "action_add_favoritos", disabled = T, style = "default")
    }
    
    # se estiver na aba 'Consultar': 
    # - desabilitar botão adicionar
    # - verificar se há algo na tabela consultar: habilitar botão remover, remover tudo, favoritos
    if(input$tab_pesquisar == "Consultar"){
      updateButton(session, "action_add_consulta", disabled = T, style = "default")
    }
    if(input$tab_pesquisar == "Consultar" & is.null(consultar_codigos$data)){
      updateButton(session, "action_remove_consulta", disabled = T, style = "default")
      updateButton(session, "action_removeall_consulta", disabled = T, style = "default")
      updateButton(session, "action_add_favoritos", disabled = T, style = "default")
    }else if(input$tab_pesquisar == "Consultar" & !is.null(consultar_codigos$data)){
      updateButton(session, "action_remove_consulta", disabled = F, style = "warning")
      updateButton(session, "action_removeall_consulta", disabled = F, style = "warning")
      updateButton(session, "action_add_favoritos", disabled = F, style = "danger")
    }
  })
  
  
  
  # MENU GERENCIAR FAVORITOS ------------------------------------------------------------
  
  # códigos adicionados na tabela favoritos
  codigos_favoritos <- reactiveValues()
  observe({
    observeEvent(input$action_add_favoritos,{
      if(input$tab_pesquisar == "Busca"){  # se estiver na aba 'Busca'
        codigos_favoritos$data <- unique(c(codigos_favoritos$data, busca_codigos_selecionados()))
      }else if(input$tab_pesquisar == "Consultar"){  # se estiver na aba 'Consultar'
        codigos_favoritos$data <- unique(c(codigos_favoritos$data, consultar_codigos$data[input$tabela_consultar_rows_selected]))
      }
    })
  })
  
  # criar tabela de favoritos
  output$tabela_favoritos <- renderDataTable({
    if(is.null(codigos_favoritos$data)){ NULL
    }else{
      codigos <- codigos_favoritos$data
      data <- data.frame(t(sapply(as.numeric(codigos), FUN = function(x){BETS.search(code = x, view = F)})))
      rownames(data) <- 1:nrow(data)
      colnames(data) <- c("Cód.", "Descrição", "Unid.", "Period.", "Início", "Fim", "Fonte")
      data
    }
  },  options = list(pageLength = 5, searching = T))
  
  # remover códigos selecionados na tabela favoritos
  observeEvent(input$action_remove_favoritos, {
    codigos <- codigos_favoritos$data
    data <- data.frame(t(sapply(as.numeric(codigos), FUN = function(x){BETS.search(code = x, view = F)})))
    codigos_favoritos$data <- unlist(data[-c(input$tabela_favoritos_rows_selected),"code"])
  })
  
  # remover todos os códigos da tabela favoritos
  observeEvent(input$action_removeall_favoritos, {
    codigos_favoritos$data <- NULL
  })
  
  # ativar botões de remover/remover tudo
  
  observe({
    # se estiver na aba 'Busca' e selecionou alguma linha do resultado da busca: habilitar botão adicionar e favoritos
    if(is.null(codigos_favoritos$data)){
      updateButton(session, "action_remove_favoritos", disabled = T, style = "default")
      updateButton(session, "action_removeall_favoritos", disabled = T, style = "default")
    }else{
      updateButton(session, "action_remove_favoritos", disabled = F, style = "warning")
      updateButton(session, "action_removeall_favoritos", disabled = F, style = "warning")
    }
  })
  
  # texto favoritos
  
  output$texto_favoritos <- renderUI({
    if(is.null(codigos_favoritos$data)){
      span("Nenhuma série adicionada aos favoritos.", style = "color:grey")
    }else{
      ""
    }
  })
  
  # baixar séries selecionadas da tabela favoritos
  series_favoritos <- reactive({
    selecionados <- codigos_favoritos$data[input$tabela_favoritos_rows_selected]
    periodo <- unique(unlist(data.frame(t(sapply(as.numeric(selecionados), FUN = function(x){BETS.search(code = x, view = F)})))$periodicity))
    if(length(periodo) > 1){
      list(nomes = NULL, df = NULL, st = NULL)
    }else{
      if(length(selecionados) == 1){
        st <- BETS.get(selecionados, data.frame = T)
        st <- xts(st[,2], st[,1])
        names(st) <- selecionados
        frame <- data.frame(data = as.character(index(st)), st)
        rownames(frame) <- 1:nrow(frame)
        colnames(frame) <- c("data", selecionados)
        list(nomes = selecionados, st = st, df = frame)
      }else{
        baixar.list <- lapply(selecionados, FUN = BETS.get, data.frame = T)
        names(baixar.list) <- selecionados
        st <- do.call(cbind, lapply(baixar.list, FUN = function(x) xts(x[,2],x[,1])))
        names(st) <- selecionados
        frame <- data.frame(data = as.character(index(st)), st)
        rownames(frame) <- 1:nrow(frame)
        colnames(frame) <- c("data", selecionados)
        list(nomes = selecionados, st = st, df = frame)
      }
    }
  })
  
  # habilitar botões de visualizar séries se (não) houver algo selecionado nos favoritos
  observe({
    observe({
      toggleState(id = "download_series_favoritos", condition = !(length(codigos_favoritos$data[input$tabela_favoritos_rows_selected]) == 0 | length(unique(unlist(data.frame(t(sapply(as.numeric(codigos_favoritos$data[input$tabela_favoritos_rows_selected]),
                                                                                                                                                                                       FUN = function(x){BETS.search(code = x, view = F)})))$periodicity))) > 1))
      toggleState(id = "action_ver_favoritos", condition = length(codigos_favoritos$data[input$tabela_favoritos_rows_selected]) != 0)
    })
  })

  # tabela (favoritos)
  output$ver_tabela_favoritos <- renderTable({
    tail(series_favoritos()$df, n = 12)
  })

  # gráfico (favoritos)
  grafico_favoritos <- reactive({
    if(is.null(series_favoritos()$st)){
      NULL
    }else{
      dygraph(series_favoritos()$st) %>%
        dyOptions(colors = brewer.pal(9, "Set1")) %>%
        dyRangeSelector(fillColor = "#F7F7F7") %>%
        dyLegend(labelsDiv = "legenda_grafico_favoritos", show = "always")
    }
  })
   
  output$grafico_favoritos <- renderDygraph({
    grafico_favoritos()
  })

  # > visualizar gráfico e tabela
  output$ver_favoritos <- renderUI({
    if(is.null(series_favoritos()$st)){
      span("Séries selecionadas com periodicidade diferente. Selecione apenas séries de mesma periodicidade.", style = "color:grey")
    }else{
      tabsetPanel(
        tabPanel("Gráfico",
                 fluidRow(
                   column(10, br(), dygraphOutput("grafico_favoritos")),
                   column(2, br(), textOutput("legenda_grafico_favoritos"))
                 ),
                 hr(),
                 downloadButton("download_grafico_favoritos", 'Exportar Gráfico')
        ),
        tabPanel("Dados",
                 br(),
                 span("Veja as 12 observações mais recentes:", style = "color:grey"), br(), br(),
                 tableOutput("ver_tabela_favoritos")
        )
      )
    }
  })

  observeEvent(input$action_ver_favoritos, {
    showModal(
      modalDialog(
        uiOutput("ver_favoritos"),
        title = div("Visualização de Séries Temporais", style = "font-weight:bold"),
        easyClose = TRUE, footer = modalButton("Fechar"), size = "l"
      )
    )
  })

  # baixar gráfico.html
  output$download_grafico_favoritos <- downloadHandler(
    filename = "SMARTIBRE_grafico.html",
    content = function(file) saveWidget(grafico_favoritos(), file, selfcontained = T)
  )

  output$download_series_favoritos <- downloadHandler(
    filename = "SMARTIBRE_series.csv",
    content = function(file) write.csv2(series_favoritos()$df, file, row.names = F, na = "")
  )

  # visualizar séries temporais da aba favoritos -------------------
  
  #series = paste("insert into favoritos(series) values(",input$,")")
  #dbSendQuery(conn,seires)
  
  # MENU RELATÓRIOS ----------------------------------------
  
  
})