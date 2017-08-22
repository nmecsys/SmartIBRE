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
              #fluidPage(theme = shinytheme("lumen")),
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
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("PESQUISAR", style = "font-weight:bold; color:#4D8AB2; font-size:120%"),
                        "Utilize os campos de pesquisa a seguir para encontrar séries temporais de interesse."),
              
              
             
              sidebarLayout(
                sidebarPanel(width = 3,style = "background-color:#F7F7F7;",
                             textInput("search_description", label = "Busca textual:", value = "", width = "90%"),
                             bsButton("action_search_description", label = "", icon = icon("search"), style = "primary"),
                             
                             hr(),
                             
                             numericInput("search_code", label = "Busca por código:", value = "", width = "90%"),
                             bsButton("action_search_code", label = "", icon = icon("search"), style = "primary"),
                           
                             hr(),
                             
                             selectInput("search_src", label = "Busca por centro:", multiple = F, width = "90%",
                                          choices = c("Selecione"="", "IBGE","BCB","FGV","FGV|IBRE","BCB e FGV","BCB-Deban",
                                                    "BCB-Depin","BCB-Derin","BCB-Desig","BCB-Secre","BCB-Demab",
                                                    "BCB-Denor","BCB-Depec","Sisbacen","Abecip")),
                             bsButton("action_search_src", label = "", icon = icon("search"), style = "primary")
                             
                ),
                mainPanel(width = 9,
                          wellPanel(style = "background-color:#F7F7F7;",
                                    
                                    tipify(bsButton("action_add_consulta", label = "", icon = icon("plus"), style = "primary", disabled = T), title = "Adicionar à lista de consulta", placement = "top"), HTML("&nbsp;"),
                                    tipify(bsButton("action_remove_consulta", label = "", icon = icon("minus"), style = "primary", disabled = T), title = "Remover da lista de consulta", placement = "top"), HTML("&nbsp;"),
                                    tipify(bsButton("action_add_favoritos", label = "", icon = icon("heart"), style = "danger", disabled = T), title = "Adicionar aos favoritos", placement = "top"), HTML("&nbsp;"),
                                    tipify(bsButton("action_remove_favoritos", label = "", icon = icon("trash"), style = "danger"), title = "Remover dos favoritos", placement = "top"), HTML("&nbsp;"),
                                    hr(),
                                    tabsetPanel(
                                      tabPanel("Busca", br(),
                                               conditionalPanel("input.action_search_description == 0 & input.action_search_code == 0 & input.action_search_src == 0",
                                                                span("Use o painel à esquerda para buscar séries.", style = "color:grey")),
                                               
                                               dataTableOutput("BETS_search")
                                      ),
                                      tabPanel("Consultar", br(),
                                               conditionalPanel("input.action_add_consulta == 0",
                                                                span("Nenhuma série adicionada à lista de consulta.", style = "color:grey")
                                               ),
                                               
                                               conditionalPanel("input.action_add_consulta != 0",
                                                                tipify(bsButton("action_view_consulta", label = "", icon = icon("line-chart")), title = "Visualizar gráfico e dados", placement = "top"), HTML("&nbsp;"),
                                                                #selectInput("pesquisar_exportar_data", label = "", choices = c(".txt",".csv"), width = 200),
                                                                tipify(bsButton("action_save_consulta", label = "", icon = icon("save")), title = "Exportar", placement = "top"), HTML("&nbsp;")
                                                                
                                                                
                                                                
                                                                
                                               )
                                      )
                                    )
                          )
                )
              ),
              
             
              hr()
              
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
