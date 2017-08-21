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
  

  
})