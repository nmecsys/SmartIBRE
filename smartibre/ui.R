dashboardPage(
  skin = "blue",
  title = "SMARTIBRE",
  header = dashboardHeader(title = div("SMARTIBRE", style = "font-family: sans-serif; font-weight:bold"),
                           dropdownMenuOutput("notif_user")
                           
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(id = "menu_esquerda",
                menuItem("Página Inicial", tabName = "home", icon = icon("home")),
                menuItem("Banco de séries temporais", icon = icon("database"), startExpanded = T,
                         menuSubItem("Pesquisar", tabName = "bd_pesquisar"),
                         menuSubItem("Gerenciar Favoritos", tabName = "bd_favoritos"),
                         menuSubItem("Relatórios", tabName = "bd_relatorios")
                ),
                menuItem("Modelo Paramétrico", tabName = "mod_param", icon = icon("dashboard"))
    )
  ),
  body = dashboardBody(#style = "background-color:#FFFFFF",
    shinyjs::useShinyjs(),  
    tabItems(
      
      # Item: Página inicial ---------------------------------------------------------------------------------------- 
      tabItem(tabName = "home", 
              div(style = "height: 900px",
                  div("Bem-vindo(a) ao SmartIBRE!", style = "text-align:center; font-size:120%; font-weight:bold"), br(),
                  
                  box(
                    width = 7, title = div("Dúvidas sobre o SmartIBRE? Aprenda aqui!", style = "font-weight:bold; font-size:95%"), height = "300px", 
                    box(width = 4, background = "light-blue", title = div("Tutorial 1", style = "font-weight:bold"), height = "210px"),
                    box(width = 4, background = "blue", title = div("Tutorial 2", style = "font-weight:bold"), height = "210px"),
                    box(width = 4, background = "navy", title = div("Tutorial 3", style = "font-weight:bold"), height = "210px")
                  ),
                  box(
                    width = 5, title = div("Notícia do dia", style = "font-weight:bold; font-size:95%"), height = "300px",
                    div(style = "font-weight:bold", 
                        "Brasileiro mais animado para gastar no Dia das Crianças "), 
                    div(style = "color:#707070; font-size:90%", 
                        "10-10-2017"),
                    br(),
                    div(img(src = "http://portalibre.fgv.br/lumis/portal/file/fileDownload.jsp?fileId=8A7C82C544B314F901451A3E03F378CA", height = 80),
                        style = "text-align:center"), br(),
                    div(style = "font-size:90%; text-align: justify",
                        "Os brasileiros estão mais dispostos a desembolsar com os presentes do Dia das Crianças: a média de intenção de gasto ficou em R$ 82,50 este ano, contra R$ 78,60 em 2016. Fonte: Sondagem do Consumidor (FGV IBRE)."),
                    div(style = "font-size:90%; text-align: justify",
                        a("Clique aqui", href = "https://goo.gl/iQNeKM", target = "_blank"), "e leia a matéria na íntegra.")
                  ),
                  box(
                    width = 12, title = div("Estimamos preços que você nem imagina", style = "font-weight:bold; font-size:95%"), height = "200px", 
                    box(width = 2, title = div("Preço 1", style = "font-weight:normal; font-size:80%"), height = "110px", solidHeader = T),
                    box(width = 2, title = div("Preço 2", style = "font-weight:normal; font-size:80%"), height = "110px", solidHeader = T),
                    box(width = 2, title = div("Preço 3", style = "font-weight:normal; font-size:80%"), height = "110px", solidHeader = T),
                    box(width = 2, title = div("Preço 4", style = "font-weight:normal; font-size:80%"), height = "110px", solidHeader = T),
                    box(width = 2, title = div("Preço 5", style = "font-weight:normal; font-size:80%"), height = "110px", solidHeader = T),
                    box(width = 2, title = div("Preço 6", style = "font-weight:normal; font-size:80%"), height = "110px", solidHeader = T)
                  ),
                  
                  box(
                    width = 7, title =  div("Artigo da semana", style = "font-weight:bold; font-size:95%"), height = "300px"
                    
                  ),
                  box(
                    width = 5, title = div("Conheça a FGV IBRE no Youtube", style = "font-weight:bold; font-size:95%"), height = "300px",
                    div(HTML('<iframe width="90%" height="200" src="https://www.youtube.com/embed/HvfXgGsJ4TI" frameborder="0" allowfullscreen></iframe>'),
                        style = "text-align:center; height:100%")
                  )
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
                             
                             selectInput("search_src", label = "Busca por fonte:", multiple = F, width = "90%",
                                         choices = c("Selecione"="", "IBGE","BCB","FGV","FGV|IBRE","BCB e FGV","BCB-Deban",
                                                     "BCB-Depin","BCB-Derin","BCB-Desig","BCB-Secre","BCB-Demab",
                                                     "BCB-Denor","BCB-Depec","Sisbacen","Abecip")),
                             bsButton("action_search_src", label = "", icon = icon("search"), style = "primary")
                ),
                mainPanel(width = 9,
                          wellPanel(style = "background-color:#F7F7F7;",
                                    tipify(bsButton("action_add_consulta", label = "", icon = icon("plus"), disabled = T), title = "Adicionar à lista de consulta", placement = "top"), HTML("&nbsp;"),
                                    tipify(bsButton("action_remove_consulta", label = "", icon = icon("minus"), disabled = T), title = "Remover da lista de consulta", placement = "top"), HTML("&nbsp;"),
                                    tipify(bsButton("action_removeall_consulta", label = "", icon = icon("trash"), disabled = T), title = "Remover tudo da lista de consulta", placement = "top"), HTML("&nbsp;"),
                                    tipify(bsButton("action_add_favoritos", label = "", icon = icon("heart"), disabled = T), title = "Adicionar aos favoritos", placement = "top"), HTML("&nbsp;"),
                                    hr(),
                                    tabsetPanel(id = "tab_pesquisar",
                                                tabPanel("Busca", br(),
                                                         uiOutput("texto_busca"),
                                                         dataTableOutput("BETS_search")
                                                ),
                                                tabPanel("Consultar", br(),
                                                         uiOutput("texto_consultar"),
                                                         br(),
                                                         dataTableOutput("tabela_consultar"),
                                                         hr(),
                                                         fluidRow(
                                                           column(width = 2,
                                                                  bsButton("action_ver_consultar", label = "Visualizar", icon = icon("area-chart"), disabled = T, width = "100%")
                                                           ),
                                                           column(width = 2,   
                                                                  downloadButton("download_series_consultar", 'Exportar Séries', class = "background-color:blue;")
                                                           )
                                                         )
                                                )
                                    )
                          )
                )
              ),
              
              
              hr()
              
      ), # fim do Item: Banco de séries temporais - Pesquisar
      
      
      
      # Item: Banco de séries temporais - Gerenciar Favoritos ------------------------------------------------------------ 
      tabItem(tabName = "bd_favoritos",
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("GERENCIAR FAVORITOS", style = "font-weight:bold; color:#4D8AB2; font-size:120%"),
                        "Gerencie a lista de séries classificadas como favoritas."),
              
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("SÉRIES FAVORITAS", style = "font-weight:bold; color:#4D8AB2; font-size:120%"), hr(),
                        tipify(bsButton("action_save_favoritos", label = "", icon = icon("save"), disabled = F, style = "danger"), title = "Salvar lista", placement = "top"), HTML("&nbsp;"),
                        tipify(bsButton("action_remove_favoritos", label = "", icon = icon("minus"), disabled = T), title = "Remover de favoritos", placement = "top"), HTML("&nbsp;"),
                        tipify(bsButton("action_removeall_favoritos", label = "", icon = icon("trash"), disabled = T), title = "Limpar lista de favoritos", placement = "top"), HTML("&nbsp;"),
                        hr(),
                        uiOutput("texto_favoritos"),
                        dataTableOutput("tabela_favoritos"),
                        hr(),
                        fluidRow(
                          column(width = 2,
                                 bsButton("action_ver_favoritos", label = "Visualizar", icon = icon("area-chart"), disabled = T, width = "100%")
                          ),
                          column(width = 2,   
                                 downloadButton("download_series_favoritos", 'Exportar Séries', class = "background-color:blue")
                          )
                        )
              )
              
              
      ), # fim do Item: Banco de séries temporais - Meus Favoritos
      
      
      
      # Item: Banco de séries temporais - Relatório ----------------------------------------------------------------- 
      tabItem(tabName = "bd_relatorios",
              
              "RELATORIZE"
              
      ), # fim do Item: Banco de séries temporais - Relatório
      
      
      
      # Item: Modelo Paramétrico ------------------------------------------------------------------------------------ 
      tabItem(tabName = "mod_param",
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("Custos Paramétricos", style = "font-weight:bold; color:#4D8AB2; font-size:120%"),
                        "Estima automaticamente um modelo paramétrico."),
              sidebarLayout(
                sidebarPanel(width = 3,style = "background-color:#F7F7F7;",
                             radioButtons("param_tipo", label = "Custo Paramétrico", choices = c("Regressão","Índice"), inline = T),
                             textInput("param_y", label = "Y - série resposta (código):", value = "4189", width = "90%"),
                             textInput("param_x", label = "X - séries explicativas (códigos):", value = "433,7832", width = "90%"),
                             numericInput("def_y", label = "Defasagem Y:", value = 0, min = 0, step = 1, width = "90%"),
                             numericInput("def_x", label = "Defasagem X:", value = 1, min = 1, step = 1, width = "90%"),
                             textInput("param_coefs", label = "Coeficientes covariáveis:", value = "0.7,0.3", width = "90%"),
                             bsButton("action_param", label = span("Executar", style = "font-weight:bold"), icon = icon("check"), style = "primary")
                ),
                mainPanel(width = 9,
                          wellPanel(style = "background-color:#F7F7F7;",
                                    conditionalPanel("input.param_tipo == 'Regressão'",
                                                     fluidRow(
                                                       column(10, br(), dygraphOutput("reg_param")),
                                                       column(2, br(), textOutput("legenda_grafico_reg_param"))
                                                     )
                                    ),
                                    conditionalPanel("input.param_tipo == 'Índice'",
                                                     fluidRow(
                                                       column(10, br(), dygraphOutput("ind_param")),
                                                       column(2, br(), textOutput("legenda_grafico_ind_param"))
                                                     )
                                    )
                          )
                )
              )
              
      ) # fim do Item: Modelo Paramétrico
      
      
      
    ) # fim do tabItems
  ) # fim do body
)
