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
  

  # MENU PESQUISAR -----------------------------------------
  
  BETS_search <- reactiveValues()
  observeEvent(input$action_search_code, {
    BETS_search$data <- tryCatch(BETS.search(code = input$search_code, view = F), error = function(e) NULL)
  })
  observeEvent(input$action_search_description, {
    BETS_search$data <- tryCatch(BETS.search(description = input$search_description, view = F, lang = "pt"), error = function(e) NULL) 
  })
  
  output$BETS_search <- renderDataTable({
    if(is.null(BETS_search$data)){NULL
    }else{
      x <- BETS_search$data
      colnames(x) <- c("Cód.", "Descrição", "Unid.", "Period.", "Início", "Fim", "Fonte")
      x}
  },  options = list(pageLength = 5, searching = F))
  
  # MENU MEUS FAVORITOS ------------------------------------
  
  
  
  # MENU RELATÓRIOS ----------------------------------------
  
  
})