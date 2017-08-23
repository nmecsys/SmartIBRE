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
  
  # > texto busca ------------------
  
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
  
  # > texto consultar ---------------------
  
  output$texto_consultar <- renderUI({
    if(is.null(codigos_favoritos$data)){
      span("Nenhuma série adicionada à lista de consulta.", style = "color:grey")
    }else{
      ""
    }
  })

  
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
  codigos_favoritos <- reactiveValues()
  
  consultar_codigos_selecionados <- reactive({
    consultar_codigos$data[input$tabela_consultar_rows_selected]
  })
  
  observe({
    observeEvent(input$action_add_favoritos,{
      if(input$tab_pesquisar == "Busca"){  # se estiver na aba 'Busca'
        codigos_favoritos$data <- unique(c(codigos_favoritos$data, busca_codigos_selecionados()))
      }else if(input$tab_pesquisar == "Consultar"){  # se estiver na aba 'Consultar'
        codigos_favoritos$data <- unique(c(codigos_favoritos$data, consultar_codigos_selecionados()))
      }
    })
  })
  
  # criar tabela para a aba consultar
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
  
  # remover códigos selecionados na aba consultar
  observeEvent(input$action_remove_favoritos, {
    codigos <- codigos_favoritos$data
    data <- data.frame(t(sapply(as.numeric(codigos), FUN = function(x){BETS.search(code = x, view = F)})))
    codigos_favoritos$data <- unlist(data[-c(input$tabela_favoritos_rows_selected),"code"])
  })
  
  # remover todos os códigos na aba consultar
  observeEvent(input$action_removeall_favoritos, {
    codigos_favoritos$data <- NULL
  })
  
  # > ativar botões de remover/remover tudo/salvar ------------------
  
  observe({
    # se estiver na aba 'Busca' e selecionou alguma linha do resultado da busca: habilitar botão adicionar e favoritos
    if(is.null(codigos_favoritos$data)){
      updateButton(session, "action_save_favoritos", disabled = T, style = "default")
      updateButton(session, "action_remove_favoritos", disabled = T, style = "default")
      updateButton(session, "action_removeall_favoritos", disabled = T, style = "default")
    }else{
      updateButton(session, "action_save_favoritos", disabled = F, style = "success")
      updateButton(session, "action_remove_favoritos", disabled = F, style = "warning")
      updateButton(session, "action_removeall_favoritos", disabled = F, style = "warning")
    }
  })
  
  # > texto favoritos ---------------------
  
  output$texto_favoritos <- renderUI({
    if(is.null(codigos_favoritos$data)){
    span("Nenhuma série adicionada aos favoritos.", style = "color:grey")
    }else{
    ""
    }
  })
  

  # auxiliar dfsd
  # output$linhas_consultar1 <- renderPrint({ busca_codigos_selecionados()})
  # output$linhas_consultar2 <- renderPrint({ consultar_codigos_selecionados() })
  # output$linhas_consultar3 <- renderPrint({ codigos_favoritos$data })
   
  #series = paste("insert into favoritos(series) values(",input$,")")
  #dbSendQuery(conn,seires)
  
  # MENU RELATÓRIOS ----------------------------------------
  
  
})