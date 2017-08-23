shinyServer(function(input, output,session){

  
  # PÁGINA INICIAL -----------------------------------------------------
  
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
  
  # NOTIFICAÇÕES PARA O USUÁRIO ----------------------------------------
  
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
  

  # MENU PESQUISAR ----------------------------------------------------------------
  
  # > aba busca ----------------------------------------
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
  output$BETS_search <- renderDataTable({
    if(is.null(BETS_search$data)){NULL
    }else{
      x <- BETS_search$data
      colnames(x) <- c("Cód.", "Descrição", "Unid.", "Period.", "Início", "Fim", "Fonte")
      x}
  },  options = list(pageLength = 5, searching = T))

  # ativar botões de adicionar/remover/favoritos
  observe({
    if(input$action_search_description != 0 | input$action_search_code != 0 | input$action_search_src != 0 ){
      updateButton(session, "action_add_consulta", disabled = F, style = "primary")
      updateButton(session, "action_remove_consulta", disabled = F, style = "warning")
      updateButton(session, "action_add_favoritos", disabled = F, style = "danger")
    }
  })
  
  # > aba consultar ----------------------------------------
  
  output$linhas_consultar1 <- renderPrint({busca_codigos_selecionados()})
  
  
  busca_codigos_selecionados <- reactive({BETS_search$data[input$BETS_search_rows_selected,"code"]})
  
  busca_codigos_consultar <- reactiveValues()
  observeEvent(input$action_add_consulta, {
    busca_codigos_consultar$data <- c(busca_codigos_consultar$data, busca_codigos_selecionados())
  })
  
  output$linhas_consultar2 <- renderPrint({ busca_codigos_consultar$data})
  #observeEvent()
  # MENU GERENCIAR FAVORITOS ------------------------------------------------------------
  
  #series = paste("insert into favoritos(series) values(",input$,")")
  #dbSendQuery(conn,seires)
  
  # MENU RELATÓRIOS ----------------------------------------
  
  
})