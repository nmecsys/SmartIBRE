dashboardPage(
  skin = "blue",
  title = "SMARTIBRE - NMECSYS",
  header = dashboardHeader(title = div("SMARTIBRE", style = "font-family: sans-serif; font-weight:bold"),
                           dropdownMenuOutput("notif_user")
                           
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(id = "menu_esquerda",
      menuItem("Página Inicial", tabName = "home", icon = icon("home")),
      menuItem("Banco de séries temporais", icon = icon("database"), startExpanded = T,
               menuSubItem("Pesquisar", tabName = "bd_pesquisar"),
               menuSubItem("Meus Favoritos", tabName = "bd_favoritos"),
               menuSubItem("Relatórios", tabName = "bd_relatorios")
               ),
      menuItem("Modelo Paramétrico", tabName = "mod_param", icon = icon("dashboard"))
    )
  ),
  body = dashboardBody(#style = "background-color:#FFFFFF",
    tabItems(
      
      # Item: Página inicial ---------------------------------------------------------------------------------------- 
      tabItem(tabName = "home", 
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("Bem-vindo(a) ao SmartIBRE. Selecione o que você deseja fazer hoje.", style = "text-align:center; font-size:120%"),
                        hr(),
                        box(
                          width = 3, background = "light-blue",
                          tipify(actionLink("action_pesquisar", div("PESQUISAR SÉRIES TEMPORAIS", style = "font-weight:bold; text-align:center; color:#FFF")),"Busque e baixe séries temporais", placement = "top")
                        ),
                        box(
                          width = 3, background = "light-blue",
                          tipify(actionLink("action_favoritos", div("ACESSAR MEUS FAVORITOS", style = "font-weight:bold; text-align:center; color:#FFF")), "Acompanhe suas séries temporais favoritadas", placement = "bottom")
                        ),
                        
                        box(
                          width = 3, background = "light-blue",
                          tipify(actionLink("action_relatorios", div("CRIAR RELATÓRIOS", style = "font-weight:bold; text-align:center; color:#FFF")), "Relatório de análise de séries temporais automático", placement = "bottom")
                        ),
                        box(
                          width = 3, background = "light-blue",
                          tipify(actionLink("action_parametrico", div("PREVISÃO", style = "font-weight:bold; text-align:center; color:#FFF")), "Crie um modelo paramétrico", placement = "top")
                        ),
             
                
                br(),br(),hr()
               


                )
              
      ), # fim do Item: Página inicial
      
      
      
      # Item: Banco de séries temporais - Pesquisar ----------------------------------------------------------------- 
      tabItem(tabName = "bd_pesquisar",
              
              "PESQUISE"
              
      ), # fim do Item: Banco de séries temporais - Pesquisar
      
      # Item: Banco de séries temporais - Meus Favoritos ------------------------------------------------------------ 
      tabItem(tabName = "bd_favoritos",
              
              "FAVORITE"
              
      ), # fim do Item: Banco de séries temporais - Meus Favoritos
      
      
      # Item: Banco de séries temporais - Relatório ----------------------------------------------------------------- 
      tabItem(tabName = "bd_relatorios",
              
              "RELATORIZE"
              
      ), # fim do Item: Banco de séries temporais - Relatório
      
      
      
      # Item: Modelo Paramétrico ------------------------------------------------------------------------------------ 
      tabItem(tabName = "mod_param",
              
              "PARAMETRIZE"
              
      ) # fim do Item: Modelo Paramétrico
      
      
      
    ) # fim do tabItems
  ) # fim do body
)
