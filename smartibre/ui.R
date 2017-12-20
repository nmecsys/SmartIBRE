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
                         menuSubItem("Gerenciar Favoritos", tabName = "bd_favoritos")
                        
                ),
                menuItem("Modelo Paramétrico", tabName = "mod_param", icon = icon("dashboard")),
                menuItem("Documentos Dinâmicos", tabName = "documentos_dinamicos", icon = icon("file"), startExpanded = T,
                                  menuSubItem("Relatórios", tabName = "bd_relatorios"),
                                  menuSubItem("Dashboards", tabName = "bd_dashboards")
                                  
                         )
    )
  ),
  body = dashboardBody(#style = "background-color:#FFFFFF",
    shinyjs::useShinyjs(),  
    tabItems(
      
      # Item: Página inicial ---------------------------------------------------------------------------------------- 
      tabItem(tabName = "home", 
              div(style = "height: 1450px",
                  box(width = 12, solidHeader = T,
                      hr(), div("Bem-vindo(a) ao SmartIBRE!", style = "text-align:center; font-size:120%; font-weight:bold"), hr(),
                      
                      div("Portal IBRE - Notícias da semana", style = "font-weight:bold; background-color:#FCFCFC; font-size:115%; color:#33A1DE; padding-left:20px"), br(),
                      
                      fluidRow(
                        column(4,
                               div(style = "margin-left:20px; margin-right:20px; height:250px",
                                   #div("Notícia do dia - Portal IBRE", style = "font-weight:bold; font-size:115%; color:#33A1DE"),
                                   div(style = "font-weight:bold", "Brasileiro mais animado para gastar no Dia das Crianças "), 
                                   div(style = "color:#707070; font-size:90%", "10-10-2017"),
                                   br(),
                                   fluidRow(
                                     #column(4, div(img(src = "http://portalibre.fgv.br/lumis/portal/file/fileDownload.jsp?fileId=8A7C82C544B314F901451A3E03F378CA", height = 70, style = "border: 1px solid #33A1DE"), style = "text-align:center")),
                                     column(12, div(style = "font-size:90%; text-align: justify", "Os brasileiros estão mais dispostos a desembolsar com os presentes do Dia das Crianças: a média de intenção de gasto ficou em R$ 82,50 este ano, contra R$ 78,60 em 2016. Fonte: Sondagem do Consumidor (FGV IBRE)."),
                                            div(style = "font-size:90%; text-align: justify", a("Clique aqui", href = "https://goo.gl/iQNeKM", target = "_blank"), "e leia a matéria na íntegra."))
                                   )
                               )
                        ),
                        column(4,
                               div(style = "margin-left:20px; margin-right:20px; height:250px",
                                   #div("Notícia do dia - Portal IBRE", style = "font-weight:bold; font-size:115%; color:#33A1DE"),
                                   div(style = "font-weight:bold", "Brasileiro mais animado para gastar no Dia das Crianças "), 
                                   div(style = "color:#707070; font-size:90%", "10-10-2017"),
                                   br(),
                                   fluidRow(
                                     #column(4, div(img(src = "http://portalibre.fgv.br/lumis/portal/file/fileDownload.jsp?fileId=8A7C82C544B314F901451A3E03F378CA", height = 70, style = "border: 1px solid #33A1DE"), style = "text-align:center")),
                                     column(12, div(style = "font-size:90%; text-align: justify", "Os brasileiros estão mais dispostos a desembolsar com os presentes do Dia das Crianças: a média de intenção de gasto ficou em R$ 82,50 este ano, contra R$ 78,60 em 2016. Fonte: Sondagem do Consumidor (FGV IBRE)."),
                                            div(style = "font-size:90%; text-align: justify", a("Clique aqui", href = "https://goo.gl/iQNeKM", target = "_blank"), "e leia a matéria na íntegra."))
                                   )
                               )
                        ),
                        column(4,
                               div(style = "margin-left:20px; margin-right:20px; height:250px",
                                   #div("Notícia do dia - Portal IBRE", style = "font-weight:bold; font-size:115%; color:#33A1DE"),
                                   div(style = "font-weight:bold", "Brasileiro mais animado para gastar no Dia das Crianças "), 
                                   div(style = "color:#707070; font-size:90%", "10-10-2017"),
                                   br(),
                                   fluidRow(
                                     #column(4, div(img(src = "http://portalibre.fgv.br/lumis/portal/file/fileDownload.jsp?fileId=8A7C82C544B314F901451A3E03F378CA", height = 70, style = "border: 1px solid #33A1DE"), style = "text-align:center")),
                                     column(12, div(style = "font-size:90%; text-align: justify", "Os brasileiros estão mais dispostos a desembolsar com os presentes do Dia das Crianças: a média de intenção de gasto ficou em R$ 82,50 este ano, contra R$ 78,60 em 2016. Fonte: Sondagem do Consumidor (FGV IBRE)."),
                                            div(style = "font-size:90%; text-align: justify", a("Clique aqui", href = "https://goo.gl/iQNeKM", target = "_blank"), "e leia a matéria na íntegra."))
                                   )
                               )
                        )
                      ),
                      hr(),
                      div("Estimamos preços que você nem imagina...", style = "font-weight:bold; background-color:#FCFCFC; font-size:115%; color:#33A1DE; padding-left:20px"),
                      
                      div(style = "margin-left:20px; margin-right:20px; height:270px",
                          
                          br(),
                          fluidRow(
                            column(2,  div(textOutput("legenda_produto_home1"), style = "font-size:75%;margin-left:30px")),
                            column(2,  div(textOutput("legenda_produto_home2"), style = "font-size:75%;margin-left:30px")),
                            column(2,  div(textOutput("legenda_produto_home3"), style = "font-size:75%;margin-left:30px")),
                            column(2,  div(textOutput("legenda_produto_home4"), style = "font-size:75%;margin-left:30px")),
                            column(2,  div(textOutput("legenda_produto_home5"), style = "font-size:75%;margin-left:30px")),
                            column(2,  div(textOutput("legenda_produto_home6"), style = "font-size:75%;margin-left:30px"))
                          ),
                          fluidRow(
                            column(2, dygraphOutput("produto_home1", height = 200, width = "97%")),
                            column(2, dygraphOutput("produto_home2", height = 200, width = "97%")),
                            column(2, dygraphOutput("produto_home3", height = 200, width = "97%")),
                            column(2, dygraphOutput("produto_home4", height = 200, width = "97%")),
                            column(2, dygraphOutput("produto_home5", height = 200, width = "97%")),
                            column(2, dygraphOutput("produto_home6", height = 200, width = "97%"))
                          )
                          
                      ),
                      hr(),
                      div("Blog do IBRE", style = "font-weight:bold; background-color:#FCFCFC; font-size:115%; color:#33A1DE; padding-left:20px"), br(),
                      fluidRow(
                        column(4, 
                               div(style = "margin-left:20px; margin-right:20px; height:250px",
                                   div(style = "font-weight:bold", textOutput("bi_noticia1_manc")), 
                                   div(style = "color:#707070; font-size:90%", textOutput("bi_noticia1_date")),
                                   br(),
                                   fluidRow(
                                     #column(4, div(img(src = "http://blogdoibre.fgv.br/sites/blogdoibre.fgv.br/themes/libre/logo.png", height = 40), style = "text-align:center")),
                                     column(12, div(style = "font-size:90%; text-align: justify", textOutput("bi_noticia1_desc")),
                                            div(style = "font-size:90%; text-align: justify", a("Clique aqui", href = "http://blogdoibre.fgv.br/posts/o-debate-economico-nao-precisa-de-espantalhos", target = "_blank"), "e leia o artigo completo."))
                                   )
                               )
                        ),
                        column(4, 
                               div(style = "margin-left:20px; margin-right:20px; height:250px",
                                   div(style = "font-weight:bold", textOutput("bi_noticia2_manc")), 
                                   div(style = "color:#707070; font-size:90%", textOutput("bi_noticia2_date")),
                                   br(),
                                   fluidRow(
                                     #column(4, div(img(src = "http://blogdoibre.fgv.br/sites/blogdoibre.fgv.br/themes/libre/logo.png", height = 40), style = "text-align:center")),
                                     column(12, div(style = "font-size:90%; text-align: justify", textOutput("bi_noticia2_desc")),
                                            div(style = "font-size:90%; text-align: justify", a("Clique aqui", href = "http://blogdoibre.fgv.br/posts/o-debate-economico-nao-precisa-de-espantalhos", target = "_blank"), "e leia o artigo completo."))
                                   )
                               )
                        ),
                        column(4, 
                               div(style = "margin-left:20px; margin-right:20px; height:250px",
                                   div(style = "font-weight:bold", textOutput("bi_noticia3_manc")), 
                                   div(style = "color:#707070; font-size:90%", textOutput("bi_noticia3_date")),
                                   br(),
                                   fluidRow(
                                     #column(4, div(img(src = "http://blogdoibre.fgv.br/sites/blogdoibre.fgv.br/themes/libre/logo.png", height = 40), style = "text-align:center")),
                                     column(12, div(style = "font-size:90%; text-align: justify",textOutput("bi_noticia3_desc")),
                                            div(style = "font-size:90%; text-align: justify", a("Clique aqui", href = "http://blogdoibre.fgv.br/posts/o-debate-economico-nao-precisa-de-espantalhos", target = "_blank"), "e leia o artigo completo."))
                                   )
                               )
                        )
                        
                      ),
                      hr(),
                      div("Dúvidas sobre como usar o SmartIBRE? Aprenda aqui!", style = "font-weight:bold; background-color:#FCFCFC; font-size:115%; color:#33A1DE; padding-left:20px"), br(),
                      
                      fluidRow(
                        column(12, 
                               div(style = "margin-left:20px; margin-right:20px; height:250px",
                                   div("Tutorial 1", style = "font-weight:bold"),
                                   div("Tutorial 2", style = "font-weight:bold"),
                                   div("Tutorial 3", style = "font-weight:bold")
                               )
                        )
                      )
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
     
      
      # tabItem(tabName = "documentos_dinamicos"
      #         
      #         ),
      
      
      
      # Item: Documentos dinâmicos - Dashboards
      
      tabItem(tabName = "bd_dashboards",
              
              
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("PARÂMETROS", style = "font-weight:bold; color:#4D8AB2; font-size:120%"),
                        "Utilize os campos a seguir para determinar os parâmetros necessários para a criação do relatório."),
              
              sidebarLayout(
                sidebarPanel(width = 3,style = "background-color:#F7F7F7;",
                             selectInput("type", label = "Tipo do dashboard:", multiple = F, width = "90%",
                                         choices = c("Selecione"="", "Macro Situation", "Customizado")),
                             
                             
                             hr(),
                             
                             div(align ='center',
                                 helpText("Parâmetros adicionais:")),
                             checkboxInput("param_add", "Incluir", FALSE),
                             textInput("dashboard_autor", "Autor", value = "", width = NULL, placeholder = NULL),
                             textInput("dashboard_url", "URL", value = "", width = NULL, placeholder = NULL),
                             fileInput("dashboard_txt", "Texto Personalizado", multiple = FALSE, accept = NULL, width = NULL,
                                       buttonLabel = "Upload", placeholder = "Arquivo .txt"),
                             fileInput("dashboard_logo", "Sua Logo", multiple = FALSE, accept = NULL, width = NULL,
                                       buttonLabel = "Upload", placeholder = "Arquivo .png/jpg"),
                             hr(),
                             div(align = "center",
                                 bsButton("run_parametros_dashboard", "RUN", value = F, style = "primary"))
                           
                ),
                sidebarPanel(width = 9,
                             shiny::htmlOutput(outputId = "reltorio")
                )
                
                
              ),
              
              
              
              
              hr()
              
              
      ),
      
      # Item: Documentos dinâmicos - Relatórios
      
       tabItem(tabName = "bd_relatorios",
              
              
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("PARÂMETROS", style = "font-weight:bold; color:#4D8AB2; font-size:120%"),
                        "Utilize os campos a seguir para determinar os parâmetros necessários para a criação do dashboard"),

              sidebarLayout(
                sidebarPanel(width = 3,style = "background-color:#F7F7F7;",
                             textInput("code_ts",label = "Código da série:", value = "Ex: 21864", width = "90%"),
                              
                             hr(),
                             selectInput("mode", label = "Tipo da análise:", multiple = F, width = "90%",
                                         choices = c("Selecione"="", "SARIMA", "GRNN", "HOLT-WINTERS")),

                             hr(),
                             numericInput("lag_max", label = "Lag máximo:", value = "48", width = "90%"),
                             hr(),
                             numericInput("n_ahead", label = "Horizonte de previsão:", value = "12", width = "90%"),
                            
                             hr(),
                             div(align ='center',
                             helpText("Parâmetros adicionais:")),
                             dateInput("window_inicio", "Janela Início", value = hoje, min = NULL, max = NULL,
                                       format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                       language = "en", width = ),
                             dateInput("window_fim", "Janela Fim", value = ontem, min = NULL, max = NULL,
                                       format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                       language = "en", width = ),
                             textInput("relatorio_ur.test", "Aplicar o Ur.test", value = "Ex: list(mode = 'ADF', type = 'drift', lags = 11, selectlags = 'BIC', level = '5pct')", width = NULL, placeholder = NULL),
                             textInput("relatorio_box.test", "Aplicar o Box.test", value = "Ex: list(lag = 2)", width = NULL, placeholder = NULL),
                             textAreaInput("relatorio_dummy", "Variável Dummy", value = "start = NULL, end = NULL, frequency = 12, year = NULL, 
                                           month = NULL, quarter = NULL,  date = NULL, from = NULL, to = NULL", 
                                           width = NULL, height = 200,cols = NULL, rows = NULL, placeholder = NULL, 
                                           resize = NULL),
                             hr(),
                             div(align = "center",
                                 bsButton("run_parametros_relatorio", "RUN", value = F, style = "primary"))
                             
                    
                             

               ),
               sidebarPanel(width = 9,
                            shiny::htmlOutput(outputId = "relatorio_html")
                            #shiny::includeHTML(path="data/teste_SARIMA_21864.html")  
                          )##fim sidebarPanel
                          
                
              ),


              
              
              hr()

              
              
              
              
      ), # fim do Item: Banco de séries temporais - Relatório
      
      
      
      # Item: Modelo Paramétrico ------------------------------------------------------------------------------------ 
      tabItem(tabName = "mod_param",
              
              wellPanel(style = "background-color:#F7F7F7;",
                        div("Custos Paramétricos", style = "font-weight:bold; color:#4D8AB2; font-size:120%"),
                        "Objetivo: estimar parametricamente a variação dos custos de um produto/serviço de acordo com suas as “características”."),
              sidebarLayout(
                sidebarPanel(width = 3,style = "background-color:#F7F7F7;",
                             radioButtons("paramTipo", label = "Custo Paramétrico", choices = c("Regressao","Indice"), inline = T),
                             textInput("param_y", label = "Y - série resposta (código):", value = "4189", width = "90%"),
                             textInput("param_x", label = "X - séries explicativas (códigos):", value = "433,7832", width = "90%"),
                             numericInput("def_y", label = "Defasagem Y:", value = 0, min = 0, step = 1, width = "90%"),
                             numericInput("def_x", label = "Defasagem X:", value = 0, min = 0, step = 1, width = "90%"),
                             textInput("param_coefs", label = "Coeficientes covariáveis:", value = "0.7,0.3", width = "90%"),
                             div(bsButton("action_param", label = span("Executar", style = "font-weight:bold"), style = "primary"), style = "text-align:center")
                ),
                mainPanel(width = 9,
                          wellPanel(style = "background-color:#F7F7F7;",
                                    
                                    div(tags$b("Especificações do modelo")), br(),
                                    tags$ul(
                                      tags$li("Equação geral:",textOutput("especificacoes", inline = T)),
                                      tags$li("Variáveis:",div(tableOutput("series_names"), style = "font-size:90%")),
                                      tags$li("Modelo final:", textOutput("reg_param_mod_final", inline = T))
                                    )
                          ),
                         
                          wellPanel(style = "background-color:#F7F7F7;",
                                    
      
                                    conditionalPanel("input.paramTipo == 'Regressao'",
                                    div(tags$b("Custo Paramétrico")), br(),
                                                     fluidRow(
                                                       column(10, br(), dygraphOutput("reg_param")),
                                                       column(2, br(), textOutput("legenda_grafico_reg_param"))
                                                     )
                                    ),
                                    conditionalPanel("input.paramTipo == 'Indice'",
                                                     div(tags$b("Custo Paramétrico")), br(),
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
