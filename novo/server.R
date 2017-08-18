shinyServer(function(input, output,session){

  # NOTIFICAÇÕES PARA O USUÁRIO ----------------------------------------
  
  output$notif_user <- renderMenu({
    # dataframe com as mensagens do usuário
    # necessário tornar isso dinâmico!!!
    # mensagens em um data.frame
    mensagens <- data.frame(from = c("Joninho", "Talitha"), message = c("Oi, Benina", "Bizarro!"))
    msgs <- apply(mensagens, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    # saída da função
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  
  
})