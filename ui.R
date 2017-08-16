
dashboardPage(skin = "blue", title = "smart IBRE - FGV",
              dashboardHeader(title = img(src = "smart_IBRE.png")),
              
              # Aqui fica o menu -----------------------------------------------------------------------
              
              dashboardSidebar(
                
                sidebarMenu(
                  menuItem(span('Página Inicial', style = 'font-size:96%'), tabName = 'home', icon = icon('home')),
                  menuItem(span('Banco de ST\'s', style = 'font-size:96%'), tabName = 'st_tudo', icon = icon('table'),
                           menuSubItem(span('FGV | IBRE', style = 'font-size:96%'), tabName = 'st_ibre', icon = icon('long-arrow-right')),
                           menuSubItem(span('Brasil', style = 'font-size:96%'), tabName = 'st_brasil', icon = icon('long-arrow-right')),
                           #menuSubItem(span('América Latina', style = 'font-size:96%'), tabName = 'st_america', icon = icon('long-arrow-right')),
                           menuSubItem(span('Mundo', style = 'font-size:96%'), tabName = 'st_mundo', icon = icon('long-arrow-right')),
                           menuSubItem(span('Favoritas', style = 'font-size:96%'), tabName = 'st_favoritas', icon = icon('cube'))),
                  menuItem(span('Previsão', style = 'font-size:96%'), tabName = 'previsao', icon = icon('umbrella')),
#                   menuItem(span('Análise Macro', style = 'font-size:96%'), tabName = 'macro', icon = icon('book'),
#                            menuSubItem(span('Monitor do PIB', style = 'font-size:96%'), tabName = 'atividade', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Sondagens IBRE', style = 'font-size:96%'), tabName = 'sondagens', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Mercado de Trabalho', style = 'font-size:96%'), tabName = 'mercado', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Evolução do Crédito', style = 'font-size:96%'), tabName = 'credito', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Inflação', style = 'font-size:96%'), tabName = 'inflacao', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Política Monetária', style = 'font-size:96%'), tabName = 'monetaria', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Política Fiscal', style = 'font-size:96%'), tabName = 'fiscal', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Setor Externo', style = 'font-size:96%'), tabName = 'externo', icon = icon('long-arrow-right')),
#                            menuSubItem(span('Panorama Internacional', style = 'font-size:96%'), tabName = 'panorama', icon = icon('long-arrow-right'))
#                            # ),
                  menuItem(span('Ajuste Sazonal', style = 'font-size:96%'), tabName = "ajuste_sazonal", icon = icon('line-chart')),
                  menuItem(span('Modelo Paramétrico', style = 'font-size:96%'), tabName = 'parametrico', icon = icon('exchange')),
                  menuItem(span('Sobre', style = 'font-size:96%'), tabName = 'sobre', icon = icon('question'))
               
                )
              ),  # fim SIDEBAR
              
              
              # Aqui fica o que aparece ao clicar no menu principal ------------------------------------
              
              dashboardBody(
                
                tabItems(
                  
                  
                  # Item: Página inicial ---------------------------------------------------------------------------------------- 
                  tabItem(tabName = "home", 
                          
                          bsCollapse(id = "collapse_paginainicial", open = 1, 
                                     bsCollapsePanel(value = 1, style = "default",
                                                     title = span(icon('home'), "Bem-vindo ao Smart IBRE!", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                     
                          div("O", tags$b(tags$em("Smart"), "IBRE"), "foi elaborado com o intuito de familiarizar o usuário com as séries
                              temporais disponibilizadas pela Fundação Getúlio Vargas, mostrando sua utilidade e importância na conjuntura
                              econômica, e de oferecer ferramentas de análise de séries temporais.",style = "text-align:justify"),
                          
                          p("Navegue pelo painel à esquerda para verificar as ferramentas disponíveis.", style = "text-align:justify"),
                          p("Para mais informações sobre o", tags$b(tags$em("Smart"), "IBRE"), "acesse a seção",
                             tags$em("Sobre")," no painel à esquerda.", style = "text-align:justify"),
                                                  
                          hr(),
                          box(width = 12,  solidHeader = T, dygraphOutput("home"))
                               ))),
                  
                  # Item: Banco de ST's -> FGV|IBRE ---------------------------------------------------------------------------------------- 
                  tabItem(tabName = "st_ibre", 
                          
                          #verbatimTextOutput("a"),
                          
                          navbarPage(title = icon("search"), id = "menus", 
                                     
                                     # > menu início ----------------------------------------------------------------- 
                                     tabPanel(span("Início",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
                                              # box(status="primary",width = 12,    
                                              
                                              bsCollapse(id = "collapse_busca", open = 1,
                                                         bsCollapsePanel(value = 1, style = "default",
                                                                         title = span(icon("long-arrow-right"),"FGV | IBRE", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                                         strong("Bem-vindo ao banco de dados da FGV."),
                                                                         p("Nessa seção você pode visualizar as séries temporais produzidas pela FGV.
                                                                           Para tal finalidade, navegue pelo painel acima ou utilize a busca abaixo."),
                                                                         hr(),
                                                                         
                                                                         fluidRow(                                                                        
                                                                           column(3,selectInput("Busca",p("Formato da busca:", style = "font-size:95%"),
                                                                                                choices = c("Escolha o formato" = "", "Codigo","Palavras"))),
                                                                           column(9,strong("Digite os parâmetros da busca:",style = "font-size:95%"),                                           
                                                                                  tipify(sidebarSearchForm(textId = "searchText", buttonId = "buscat", label = "Search..."),
                                                                                         "Certifique-se de escolher a forma correta de busca.",
                                                                                         placement = "top",trigger = "hover"))
                                                                         ))
                                              ),
                                              
                                              conditionalPanel("input.buscat", 
                                                               bsCollapse(id = "collapse_resultados_busca", open = 1,
                                                                          bsCollapsePanel(value = 1, style = "primary",
                                                                                          title = span("Resultados", style = "font-size:90%;font-weight:bold"),
                                                                                          p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                            "para obter mais informações.", style = "color:#2C65A1"),
                                                                                          dataTableOutput("tabela_completa"),
                                                                                          br(),
                                                                                          fluidRow(
                                                                                            column(2,
                                                                                                   tipify(bsButton("visu_serie_busca", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                          "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                            column(2,
                                                                                                   # Botão acrescentar séries
                                                                                                   tipify(bsButton("add_busca", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                          "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                            )),
                                                                                          bsModal("modal_busca", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_serie_busca", size = "large",
                                                                                                  
                                                                                                  # Conteúdo da nova janela
                                                                                                  tabBox(width = "100%", 
                                                                                                         tabPanel("Gráfico", dygraphOutput("grafico_busca"),
                                                                                                                  hr(),
                                                                                                                  span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                                  div(tableOutput("legenda_busca"), style = "font-size:85%")),
                                                                                                         
                                                                                                         tabPanel("Dados", tableOutput("dados_busca")),
                                                                                                         tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_busca"), style = "font-size:96%")),
                                                                                                         tabPanel("Download",
                                                                                                                  p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                  selectInput("formato_fgv_busca", div("Formato:", style = "font-size:90%"),
                                                                                                                              choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                              selected = ".csv" ),
                                                                                                                  downloadButton('download_fgv_busca', 'Download'))
                                                                                                         
                                                                                                  )))))
                                              
                                     ),
                                     # > menu IGP ----------------------------------------------------------------- 
                                     tabPanel(span("IGP",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
                                              
                                              bsCollapse(id = "collapse_igp", open = 1,
                                                         bsCollapsePanel(value = 1, style = "default",
                                                                         title = span("Índice Geral de Preços (IGP):", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                                         p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                           "para obter mais informações.", style = "color:#2C65A1"),
                                                                         # Tabela com as séries disponíveis
                                                                         dataTableOutput("tabela_igp"),
                                                                         br(),
                                                                         
                                                                         fluidRow(
                                                                           column(2,
                                                                                  # Botão visualizar séries
                                                                                  tipify(bsButton("visu_series", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                         "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                           column(2,
                                                                                  # Botão acrescentar séries
                                                                                  tipify(bsButton("add_igp", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                         "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                           )),
                                                                         # Nova janela após clicar no botão de visualizar séries
                                                                         bsModal("modal_igp", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series", size = "large",
                                                                                 
                                                                                 # Conteúdo da nova janela
                                                                                 tabBox(width = "100%", 
                                                                                        tabPanel("Gráfico", dygraphOutput("grafico_igp"),
                                                                                                 hr(),
                                                                                                 span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                 div(tableOutput("legenda_igp"), style = "font-size:85%")),                        
                                                                                        tabPanel("Dados", tableOutput("dados_igp")),
                                                                                        tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_igp"), style = "font-size:96%")),
                                                                                        tabPanel("Download",
                                                                                                 p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                 selectInput("formato_fgv_igp", div("Formato:", style = "font-size:90%"),
                                                                                                             choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                             selected = ".csv" ),
                                                                                                 downloadButton('download_fgv_igp', 'Download'))
                                                                                 )
                                                                         )                                           
                                                         ))), # FIM MENU IGP
                                     
                                     # > menu IPA ----------------------------------------------------------------- 
                                     
                                     # > menu IPA ---------------------------------------------------------------------
#                                      tabPanel(span("IPA",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
#                                               bsCollapse(id = "collapse_ipA_filtros", open = 1,
#                                                          bsCollapsePanel(value = 1, style = "default",
#                                                                          title = span("Índice de Preços ao Produtor Amplo (IPA)", style = "font-size:90%;font-weight:bold;color:#2C65A1"),# span("IPC - 10", style = "color:black")),
#                                                                          
#                                                                          fluidRow(
#                                                                            box(width = "100%", solidHeader = T, 
#                                                                                column(2,infoBox("", icon = icon("usd"), color = "light-blue")),
#                                                                                column(3,
#                                                                                       selectInput("ipa_indice", label = "Índice:", choices = c("Selecione o tipo de IPA" = '', "IPA-10", "IPA-DI", "IPA-M"), 
#                                                                                                   selected = NULL)),
#                                                                                column(3,
#                                                                                       selectInput("ipa_estrutura", label = "Estrutura:", choices = c("Selecione a estrutura" = '',ipa_estruturas), 
#                                                                                                   selected = NULL)),
#                                                                                column(3,
#                                                                                       selectInput("ipa_unidade", label = "Unidade:", choices = c("Selecione a unidade" = '', ipa_unidades), 
#                                                                                                   selected = NULL))
#                                                                            )),
#                                                                          
#                                                                          # Estágios IPA dependendo da estrutura selecionada acima:
#                                                                          conditionalPanel("(input.ipa_indice != '') & (input.ipa_estrutura != '')  & (input.ipa_unidade != '')",
#                                                                                           fluidRow(
#                                                                                             column(6, uiOutput("ipa10_estagios")),
#                                                                                             column(6, uiOutput("ipa10_grupos"))),
#                                                                                           fluidRow(
#                                                                                             column(6, uiOutput("ipa10_subgrupos")),
#                                                                                             column(6, uiOutput("ipa10_item")))
#                                                                          ),
#                                                                          actionButton("visu_ipa10", label = "Buscar", icon = icon("search")),
#                                                                          conditionalPanel("input.visu_ipa10",
#                                                                                           bsCollapse(id = "collapse_ipa10_resultados", open = 1,
#                                                                                                      bsCollapsePanel(value = 1,span("Tabela - IPA-10", style = "font-size:90%;font-weight:bold"), style = "primary",
#                                                                                                                      p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
#                                                                                                                        "para obter mais informações.", style = "color:#2C65A1"),
#                                                                                                                      # tabela
#                                                                                                                      dataTableOutput("filtro_ipa10"),
#                                                                                                                      br(),
#                                                                                                                      fluidRow(
#                                                                                                                        column(2,
#                                                                                                                               # Botão de visualizar séries
#                                                                                                                               tipify(bsButton("visu_series_ipa10", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
#                                                                                                                                      "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
#                                                                                                                        column(2,
#                                                                                                                               # Botão acrescentar séries
#                                                                                                                               tipify(bsButton("add_ipa10", label = "Adicionar ao carrinho", icon = icon("shopping-cart"), size = "small"),
#                                                                                                                                      "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
#                                                                                                                        )),
#                                                                                                                      # Nova janela após clicar no botão
#                                                                                                                      bsModal("modal_ipa10", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_ipa10", size = "large",
#                                                                                                                              
#                                                                                                                              # Conteúdo da nova janela
#                                                                                                                              tabBox(width = "100%", 
#                                                                                                                                     tabPanel("Gráfico", dygraphOutput("grafico_ipa10"),
#                                                                                                                                              hr(),
#                                                                                                                                              span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
#                                                                                                                                              div(tableOutput("legenda_ipa10"), style = "font-size:85%")),                        
#                                                                                                                                     tabPanel("Dados", tableOutput("dados_ipa10")),
#                                                                                                                                     tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_ipa10"), style = "font-size:96%")),
#                                                                                                                                     tabPanel("Download",
#                                                                                                                                              p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
#                                                                                                                                              selectInput("formato_fgv_ipa10", div("Formato:", style = "font-size:90%"),
#                                                                                                                                                          choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
#                                                                                                                                                          selected = ".csv" ),
#                                                                                                                                              downloadButton('download_fgv_ipa10', 'Download'))
#                                                                                                                                     
#                                                                                                                              ))
#                                                                                                      ))
#                                                                          )
#                                                                          
#                                                                          ))),
                                     
                                     navbarMenu(span("IPA",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
                                                tabPanel("IPA-10",
                                                         bsCollapse(id = "collapse_ipa10_filtros", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Índice de Preços ao Produtor Amplo (IPA):", style = "font-size:90%;font-weight:bold;color:#2C65A1", span("IPA - 10", style = "color:black")),
                                                                                    
                                                                           
                                                                                    # Estrutura IPA
                                                                                    selectInput("ipa10_estrutura", label = "Estrutura:", width = "50%", 
                                                                                                choices = c('Selecione uma estrutura' = '', "Estágios de Processamento (EP)", "Origem (OG)")),
                                                                                    # Estágios IPA dependendo da estrutura selecionada acima:
                                                                                    conditionalPanel("input.ipa10_estrutura", 
                                                                                                     uiOutput("ipa10_estagios"),
                                                                                                     conditionalPanel("input.ipa10_estagios != 'TODOS OS ESTÁGIOS (MAIOR NÍVEL)'", 
                                                                                                                      uiOutput("ipa10_grupos"),
                                                                                                                      conditionalPanel("(input.ipa10_grupos != 'TODOS OS GRUPOS (MAIOR NÍVEL)') & (input.ipa10_estagios != 'PRODUTOS AGROPECUÁRIOS') & (input.ipa10_grupos != 'EMBALAGENS') & (input.ipa10_grupos != 'MATERIAIS E COMPONENTES PARA A CONSTRUÇÃO') & (input.ipa10_grupos != 'COMBUSTÍVEIS E LUBRIFICANTES PARA A PRODUÇÃO') & (input.ipa10_grupos != 'MINERAIS')",
                                                                                                                                       uiOutput("ipa10_subgrupos"),
                                                                                                                                       conditionalPanel("(input.ipa10_subgrupos != 'TODOS OS SUBGRUPOS (MAIOR NÍVEL)') & (input.ipa10_subgrupos != 'COMBUSTÍVEIS') & (input.ipa10_grupos != 'BENS DE CONSUMO DURÁVEIS') & (input.ipa10_grupos != 'BENS DE INVESTIMENTO') & (input.ipa10_grupos != 'MATÉRIAS-PRIMAS BRUTAS - AGROPECUÁRIAS') & (input.ipa10_subgrupos != 'VESTUÁRIO, CALÇADOS E ACESSÓRIOS') & (input.ipa10_subgrupos != 'MATERIAIS PARA A MANUFATURA') & (input.ipa10_subgrupos != 'SUPRIMENTOS NÃO AGROPECUÁRIOS') & (input.ipa10_subgrupos != 'PRODUTOS DO FUMO') & (input.ipa10_subgrupos != 'CARVÃO MINERAL' & (input.ipa10_subgrupos != 'OUTROS MINERAIS NÃO-METÁLICOS'))",
                                                                                                                                                        uiOutput("ipa10_item"))))),
                                                                                    actionButton("visu_ipa10", label = "Buscar", icon = icon("search"))
                                                                                    
                                                                    )),
                                                         conditionalPanel("input.visu_ipa10",
                                                                          bsCollapse(id = "collapse_ipa10_resultados", open = 1,
                                                                                     bsCollapsePanel(value = 1,span("Tabela - IPA-10", style = "font-size:90%;font-weight:bold"), style = "primary",
                                                                                                     p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                                       "para obter mais informações.", style = "color:#2C65A1"),
                                                                                                     # tabela
                                                                                                     dataTableOutput("filtro_ipa10"),
                                                                                                     br(),
                                                                                                     fluidRow(
                                                                                                       column(2,
                                                                                                              # Botão de visualizar séries
                                                                                                              tipify(bsButton("visu_series_ipa10", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                                     "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                                       column(2,
                                                                                                              # Botão acrescentar séries
                                                                                                              tipify(bsButton("add_ipa10", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                                     "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                                       )),
                                                                                                     # Nova janela após clicar no botão
                                                                                                     bsModal("modal_ipa10", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_ipa10", size = "large",
                                                                                                             
                                                                                                             # Conteúdo da nova janela
                                                                                                             tabBox(width = "100%", 
                                                                                                                    tabPanel("Gráfico", dygraphOutput("grafico_ipa10"),
                                                                                                                             hr(),
                                                                                                                             span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             div(tableOutput("legenda_ipa10"), style = "font-size:85%")),                        
                                                                                                                    tabPanel("Dados", tableOutput("dados_ipa10")),
                                                                                                                    tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_ipa10"), style = "font-size:96%")),
                                                                                                                    tabPanel("Download",
                                                                                                                             p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             selectInput("formato_fgv_ipa10", div("Formato:", style = "font-size:90%"),
                                                                                                                                         choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                                         selected = ".csv" ),
                                                                                                                             downloadButton('download_fgv_ipa10', 'Download'))
                                                                                                             
                                                                                                             ))
                                                                                                     ))
                                                                          )
                                                         
                                                         
                                                         
                                                ), # FIM IPA-10
                                                
                                                # INÍCIO IPA-DI
                                                tabPanel("IPA-DI",
                                                         
                                                         bsCollapse(id = "collapse_ipadi_filtros", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Índice de Preços ao Produtor Amplo (IPA):", style = "font-size:90%;font-weight:bold;color:#2C65A1", span("IPA - DI", style = "color:black")),
                                                                                    
                                                                                    # Estrutura IPA
                                                                                    selectInput("ipadi_estrutura", label = "Estrutura:", width = "50%", 
                                                                                                choices = c('Selecione uma estrutura' = '', "Estágios de Processamento (EP)", "Origem (OG)")),
                                                                                    
                                                                                    # Estágios IPA dependendo da estrutura selecionada acima:
                                                                                    conditionalPanel("input.ipadi_estrutura", 
                                                                                                     uiOutput("ipadi_estagios"),
                                                                                                     conditionalPanel("input.ipadi_estagios != 'TODOS OS ESTÁGIOS (MAIOR NÍVEL)'", 
                                                                                                                      uiOutput("ipadi_grupos"),
                                                                                                                      conditionalPanel("(input.ipadi_grupos != 'TODOS OS GRUPOS (MAIOR NÍVEL)') & (input.ipadi_estagios != 'PRODUTOS AGROPECUÁRIOS') & (input.ipadi_grupos != 'EMBALAGENS') & (input.ipadi_grupos != 'MATERIAIS E COMPONENTES PARA A CONSTRUÇÃO') & (input.ipadi_grupos != 'COMBUSTÍVEIS E LUBRIFICANTES PARA A PRODUÇÃO') & (input.ipadi_grupos != 'MINERAIS')",
                                                                                                                                       uiOutput("ipadi_subgrupos"),
                                                                                                                                       conditionalPanel("(input.ipadi_subgrupos != 'TODOS OS SUBGRUPOS (MAIOR NÍVEL)') & (input.ipadi_subgrupos != 'COMBUSTÍVEIS') & (input.ipadi_grupos != 'BENS DE CONSUMO DURÁVEIS') & (input.ipadi_grupos != 'BENS DE INVESTIMENTO') & (input.ipadi_grupos != 'MATÉRIAS-PRIMAS BRUTAS - AGROPECUÁRIAS') & (input.ipadi_subgrupos != 'VESTUÁRIO, CALÇADOS E ACESSÓRIOS') & (input.ipadi_subgrupos != 'MATERIAIS PARA A MANUFATURA') & (input.ipadi_subgrupos != 'SUPRIMENTOS NÃO AGROPECUÁRIOS') & (input.ipadi_subgrupos != 'PRODUTOS DO FUMO') & (input.ipadi_subgrupos != 'CARVÃO MINERAL' & (input.ipadi_subgrupos != 'OUTROS MINERAIS NÃO-METÁLICOS'))",
                                                                                                                                                        uiOutput("ipadi_item"))))),
                                                                                    actionButton("visu_ipadi", label = "Buscar", icon = icon("search"))
                                                                                    
                                                                    )),
                                                         conditionalPanel("input.visu_ipadi",
                                                                          bsCollapse(id = "collapse_ipadi_resultados", open = 1,
                                                                                     bsCollapsePanel(value = 1,span("Tabela - IPA-DI", style = "font-size:90%;font-weight:bold"), style = "primary",
                                                                                                     p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                                       "para obter mais informações.", style = "color:#2C65A1"),
                                                                                                     # tabela
                                                                                                     dataTableOutput("filtro_ipadi"),
                                                                                                     br(),
                                                                                                     fluidRow(
                                                                                                       column(2,
                                                                                                              # Botão de visualizar séries
                                                                                                              tipify(bsButton("visu_series_ipadi", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                                     "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                                       column(2,
                                                                                                              # Botão acrescentar séries
                                                                                                              tipify(bsButton("add_ipadi", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                                     "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                                       )),
                                                                                                     # Nova janela após clicar no botão
                                                                                                     bsModal("modal_ipadi", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_ipadi", size = "large",
                                                                                                             
                                                                                                             # Conteúdo da nova janela
                                                                                                             tabBox(width = "100%", 
                                                                                                                    tabPanel("Gráfico", dygraphOutput("grafico_ipadi"),
                                                                                                                             hr(),
                                                                                                                             span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             div(tableOutput("legenda_ipadi"), style = "font-size:85%")),                        
                                                                                                                    tabPanel("Dados", tableOutput("dados_ipadi")),
                                                                                                                    tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_ipadi"), style = "font-size:96%")),
                                                                                                                    tabPanel("Download",
                                                                                                                             p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             selectInput("formato_fgv_ipadi", div("Formato:", style = "font-size:90%"),
                                                                                                                                         choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                                         selected = ".csv" ),
                                                                                                                             downloadButton('download_fgv_ipadi', 'Download'))
                                                                                                                    
                                                                                                             ))
                                                                                     ))
                                                         )
                                                         
                                                ), # FIM IPA-DI,
                                                # Início IPA-M
                                                tabPanel("IPA-M",
                                                         
                                                         bsCollapse(id = "collapse_ipam_filtros", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Índice de Preços ao Produtor Amplo (IPA):", style = "font-size:90%;font-weight:bold;color:#2C65A1", span("IPA - M", style = "color:black")),
                                                                                    
                                                                                    # Estrutura IPA
                                                                                    selectInput("ipam_estrutura", label = "Estrutura:", width = "50%", 
                                                                                                choices = c('Selecione uma estrutura' = '', "Estágios de Processamento (EP)", "Origem (OG)")),
                                                                                    
                                                                                    # Estágios IPA dependendo da estrutura selecionada acima:
                                                                                    conditionalPanel("input.ipam_estrutura", 
                                                                                                     uiOutput("ipam_estagios"),
                                                                                                     conditionalPanel("input.ipam_estagios != 'TODOS OS ESTÁGIOS (MAIOR NÍVEL)'", 
                                                                                                                      uiOutput("ipam_grupos"),
                                                                                                                      conditionalPanel("(input.ipam_grupos != 'TODOS OS GRUPOS (MAIOR NÍVEL)') & (input.ipam_estagios != 'PRODUTOS AGROPECUÁRIOS') & (input.ipam_grupos != 'EMBALAGENS') & (input.ipam_grupos != 'MATERIAIS E COMPONENTES PARA A CONSTRUÇÃO') & (input.ipam_grupos != 'COMBUSTÍVEIS E LUBRIFICANTES PARA A PRODUÇÃO') & (input.ipam_grupos != 'MINERAIS')",
                                                                                                                                       uiOutput("ipam_subgrupos"),
                                                                                                                                       conditionalPanel("(input.ipam_subgrupos != 'TODOS OS SUBGRUPOS (MAIOR NÍVEL)') & (input.ipam_subgrupos != 'COMBUSTÍVEIS') & (input.ipam_grupos != 'BENS DE CONSUMO DURÁVEIS') & (input.ipam_grupos != 'BENS DE INVESTIMENTO') & (input.ipam_grupos != 'MATÉRIAS-PRIMAS BRUTAS - AGROPECUÁRIAS') & (input.ipam_subgrupos != 'VESTUÁRIO, CALÇADOS E ACESSÓRIOS') & (input.ipam_subgrupos != 'MATERIAIS PARA A MANUFATURA') & (input.ipam_subgrupos != 'SUPRIMENTOS NÃO AGROPECUÁRIOS') & (input.ipam_subgrupos != 'PRODUTOS DO FUMO') & (input.ipam_subgrupos != 'CARVÃO MINERAL' & (input.ipam_subgrupos != 'OUTROS MINERAIS NÃO-METÁLICOS'))",
                                                                                                                                                        uiOutput("ipam_item"))))),
                                                                                    actionButton("visu_ipam", label = "Buscar", icon = icon("search"))
                                                                                    
                                                                    )),
                                                         conditionalPanel("input.visu_ipam",
                                                                          bsCollapse(id = "collapse_ipam_resultados", open = 1,
                                                                                     bsCollapsePanel(value = 1,span("Tabela - IPA-M", style = "font-size:90%;font-weight:bold"), style = "primary",
                                                                                                     p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                                       "para obter mais informações.", style = "color:#2C65A1"),
                                                                                                     # tabela
                                                                                                     dataTableOutput("filtro_ipam"),
                                                                                                     br(),
                                                                                                     fluidRow(
                                                                                                       column(2,
                                                                                                              # Botão de visualizar séries
                                                                                                              tipify(bsButton("visu_series_ipam", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                                     "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                                       column(2,
                                                                                                              # Botão acrescentar séries
                                                                                                              tipify(bsButton("add_ipam", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                                     "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                                       )),
                                                                                                     # Nova janela após clicar no botão
                                                                                                     bsModal("modal_ipam", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_ipam", size = "large",
                                                                                                             
                                                                                                             # Conteúdo da nova janela
                                                                                                             tabBox(width = "100%",
                                                                                                                    tabPanel("Gráfico", dygraphOutput("grafico_ipam"),
                                                                                                                             hr(),
                                                                                                                             span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             div(tableOutput("legenda_ipam"), style = "font-size:85%")),                        
                                                                                                                    tabPanel("Dados", tableOutput("dados_ipam")),
                                                                                                                    tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_ipam"), style = "font-size:96%")),
                                                                                                                    tabPanel("Download",
                                                                                                                             p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             selectInput("formato_fgv_ipam", div("Formato:", style = "font-size:90%"),
                                                                                                                                         choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                                         selected = ".csv" ),
                                                                                                                             downloadButton('download_fgv_ipam', 'Download'))
                                                                                                                    
                                                                                                             )
                                                                                                             
                                                                                                     )
                                                                                     ))
                                                         )
                                                         
                                                ) # FIM IPA-M
                                     ), # FIM IPA
                                     
                                     # > menu IPC ---------------------------------------------------------------------
                                     tabPanel(span("IPC",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
                                                # ÍNICIO IPC-10 
                                                #tabPanel("IPC-10",
                                                         bsCollapse(id = "collapse_ipc_filtros", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Índice de Preços ao Consumidor (IPC)", style = "font-size:90%;font-weight:bold;color:#2C65A1"),# span("IPC - 10", style = "color:black")),
                                                                                    fluidRow(
                                                                                      box(width = "100%", solidHeader = T, 
                                                                                          #column(2,infoBox("", icon = icon("usd"), color = "light-blue")),
                                                                                          column(3,
                                                                                                 selectInput("ipc_indice", label = "Índice:", choices = c("Selecione o tipo de IPC" = '', "IPC-10", "IPC-DI", "IPC-M", "IPC-S"), 
                                                                                                             selected = NULL)),
                                                                                          column(3,
                                                                                                 selectInput("ipc_regiao", label = "Abrangência Geográfica:", choices = c("Selecione a abrangência" = '', ipc_regioes), 
                                                                                                             selected = NULL)),
                                                                                          column(3,
                                                                                                 selectInput("ipc_unidade", label = "Unidade:", choices = c("Selecione a unidade" = '',ipc_unidades), 
                                                                                                             selected = NULL))
                                                                                      )),
                                                                                    
                                                                                    conditionalPanel("(input.ipc_indice != '') & (input.ipc_regiao != '')  & (input.ipc_unidade != '')",
                                                                                                     fluidRow(
                                                                                                       column(6, uiOutput("ipc_grupos")),
                                                                                                       column(6, uiOutput("ipc_subgrupos"))),
                                                                                                     fluidRow(
                                                                                                       column(6, uiOutput("ipc_itens")),
                                                                                                       column(6, uiOutput("ipc_subitens")))
                                                                                    ),
                                                                                                   
#                                                         
                                                                                    actionButton("visu_ipc", label = "Buscar", icon = icon("search"))
                                                                                    
                                                                    )),
                                                         conditionalPanel("input.visu_ipc",
                                                                          bsCollapse(id = "collapse_ipc_resultados", open = 1,
                                                                                     bsCollapsePanel(value = 1,span("Tabela - IPC", style = "font-size:90%;font-weight:bold"), style = "primary",
                                                                                                     p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                                       "para obter mais informações.", style = "color:#2C65A1"),
                                                                                                     # tabela
                                                                                                     dataTableOutput("tabela_ipc"),
                                                                                                     br(),
                                                                                                     fluidRow(
                                                                                                       column(2,
                                                                                                              # Botão de visualizar séries
                                                                                                              tipify(bsButton("visu_series_ipc", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                                     "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                                       column(2,
                                                                                                              # Botão acrescentar séries
                                                                                                              tipify(bsButton("add_ipc", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                                     "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                                       )),
                                                                                                     # Nova janela após clicar no botão
                                                                                                     bsModal("modal_ipc", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_ipc", size = "large",
                                                                                                             
                                                                                                             # Conteúdo da nova janela
                                                                                                             tabBox(width = "100%", 
                                                                                                                    tabPanel("Gráfico", dygraphOutput("grafico_ipc"),
                                                                                                                             hr(),
                                                                                                                             span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             div(tableOutput("legenda_ipc"), style = "font-size:85%")),                        
                                                                                                                    tabPanel("Dados", tableOutput("dados_ipc")),
                                                                                                                    tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_ipc"), style = "font-size:96%")),
                                                                                                                    tabPanel("Download",
                                                                                                                             p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                             selectInput("formato_fgv_ipc", div("Formato:", style = "font-size:90%"),
                                                                                                                                         choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                                         selected = ".csv" ),
                                                                                                                             downloadButton('download_fgv_ipc', 'Download'))
                                                                                                                    
                                                                                                             ))
                                                                                     ))
                                                         )
                                                         
                                                         
                                                         
                                              #  ) # FIM IPC-10
#                                                 tabPanel("IPC-DI"),
#                                                 tabPanel("IPC-M"),
#                                                 tabPanel("IPC-S")
                                     ), # FIM IPC
                                     
                                     # > menu  INCC ---------------------------------------------------------------------
                                     navbarMenu(span("INCC",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
                                                tabPanel("INCC-10",
                                                         bsCollapse(id = "collapse_incc10_filtros", open = 1,
                                                           bsCollapsePanel(value = 1, style = "default",
                                                                           title = span("Índice Nacional do Custo da Construção (INCC):", style = "font-size:90%;font-weight:bold;color:#2C65A1", span("INCC - 10", style = "color:black")),
                                                                           
                                                                           
                                                                           # Estrutura IPA
#                                                                            selectInput("ipa10_estrutura", label = "Estrutura:", width = "50%", 
#                                                                                        choices = c('Selecione uma estrutura' = '', "Estágios de Processamento (EP)", "Origem (OG)")),
#                                                                            # Estágios IPA dependendo da estrutura selecionada acima:
#                                                                            conditionalPanel("input.ipa10_estrutura", 
                                                                                            uiOutput("incc10_estagio"),
                                                                                            conditionalPanel("input.incc10_estagios != 'TODOS OS ESTÁGIOS (MAIOR NÍVEL)'",
                                                                                                             uiOutput("incc10_grupos"),
                                                                                                             conditionalPanel("input.incc10_grupos != 'TODOS OS GRUPOS (MAIOR NÍVEL)'",
                                                                                                                              uiOutput("incc10_subgrupos"),
                                                                                                                              conditionalPanel("input.incc10_subgrupos != 'TODOS OS SUBGRUPOS (MAIOR NÍVEL)'",
                                                                                                                                               uiOutput("incc10_itens")))),
                                                                                            actionButton("visu_incc10", label = "Buscar", icon = icon("search")),
                                                                                          
                                                          conditionalPanel("input.visu_incc10",
                                                                           bsCollapse(id = "collapse_ipa10_resultados", open = 1,
                                                                                      bsCollapsePanel(value = 1,span("Tabela - INCC-10", style = "font-size:90%;font-weight:bold"), style = "primary",
                                                                                                      p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                                        "para obter mais informações.", style = "color:#2C65A1"),
                                                                                                      # tabela
                                                                                                      dataTableOutput("filtro_incc10"),
                                                                                      br(),
                                                                                      fluidRow(
                                                                                        column(2,
                                                                                               # Botão de visualizar séries
                                                                                               tipify(bsButton("visu_series_incc10", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                      "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                        column(2,
                                                                                               # Botão acrescentar séries
                                                                                               tipify(bsButton("add_incc10", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                      "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                        )),
                                                                                      # Nova janela após clicar no botão
                                                                                      bsModal("modal_incc10", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_incc10", size = "large",
                                                                                              
                                                                                              # Conteúdo da nova janela
                                                                                              tabBox(width = "100%", 
                                                                                                     tabPanel("Gráfico", dygraphOutput("grafico_incc10")),
                                                                                                     tabPanel("Dados", tableOutput("dados_incc10")),
                                                                                                     tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_incc10"), style = "font-size:96%")),
                                                                                                     tabPanel("Download",
                                                                                                              p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                              selectInput("formato_fgv_incc10", div("Formato:", style = "font-size:90%"),
                                                                                                                          choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                          selected = ".csv" ),
                                                                                                              downloadButton('download_fgv_incc10', 'Download')))))))


                                                                                      ))),
                                                
                                                #tabPanel("INCC-DI"),
                                                tabPanel("INCC-DI",
                                                         bsCollapse(id = "collapse_inccdi_filtros", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Índice Nacional do Custo da Construção (INCC):", style = "font-size:90%;font-weight:bold;color:#2C65A1", span("INCC - 10", style = "color:black")),
                                                                                    
                                                                                    
                                                                                    # Estrutura IPA
                                                                                    #                                                                            selectInput("ipa10_estrutura", label = "Estrutura:", width = "50%", 
                                                                                    #                                                                                        choices = c('Selecione uma estrutura' = '', "Estágios de Processamento (EP)", "Origem (OG)")),
                                                                                    #                                                                            # Estágios IPA dependendo da estrutura selecionada acima:
                                                                                    #                                                                            conditionalPanel("input.ipa10_estrutura", 
                                                                                    uiOutput("inccdi_estagio"),
                                                                                    conditionalPanel("input.inccdi_estagios != 'TODOS OS ESTÁGIOS (MAIOR NÍVEL)'",
                                                                                                     uiOutput("inccdi_grupos"),
                                                                                                     conditionalPanel("input.inccdi_grupos != 'TODOS OS GRUPOS (MAIOR NÍVEL)'",
                                                                                                                      uiOutput("inccdi_subgrupos"),
                                                                                                                      conditionalPanel("input.inccdi_subgrupos != 'TODOS OS SUBGRUPOS (MAIOR NÍVEL)'",
                                                                                                                                       uiOutput("inccdi_itens")))),
                                                                                    actionButton("visu_inccdi", label = "Buscar", icon = icon("search")),
                                                                                    
                                                                                    conditionalPanel("input.visu_inccdi",
                                                                                                     bsCollapse(id = "collapse_ipa10_resultados", open = 1,
                                                                                                                bsCollapsePanel(value = 1,span("Tabela - INCC-10", style = "font-size:90%;font-weight:bold"), style = "primary",
                                                                                                                                p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                                                                  "para obter mais informações.", style = "color:#2C65A1"),
                                                                                                                                # tabela
                                                                                                                                dataTableOutput("filtro_inccdi"),
                                                                                                                                br(),
                                                                                                                                fluidRow(
                                                                                                                                  column(2,
                                                                                                                                         # Botão de visualizar séries
                                                                                                                                         tipify(bsButton("visu_series_inccdi", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                                                                "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                                                                  column(2,
                                                                                                                                         # Botão acrescentar séries
                                                                                                                                         tipify(bsButton("add_inccdi", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                                                                "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                                                                  )),
                                                                                                                                # Nova janela após clicar no botão
                                                                                                                                bsModal("modal_inccdi", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_inccdi", size = "large",
                                                                                                                                        
                                                                                                                                        # Conteúdo da nova janela
                                                                                                                                        tabBox(width = "100%", 
                                                                                                                                               tabPanel("Gráfico", dygraphOutput("grafico_inccdi")),
                                                                                                                                               tabPanel("Dados", tableOutput("dados_inccdi")),
                                                                                                                                               tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_inccdi"), style = "font-size:96%")),
                                                                                                                                               tabPanel("Download",
                                                                                                                                                        p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                                                        selectInput("formato_fgv_inccdi", div("Formato:", style = "font-size:90%"),
                                                                                                                                                                    choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                                                                    selected = ".csv" ),
                                                                                                                                                        downloadButton('download_fgv_inccdi', 'Download')))))))
                                                                                    
                                                                                    
                                                                    ))),
                                                tabPanel("INCC-M",
                                                         bsCollapse(id = "collapse_inccm_filtros", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Índice Nacional do Custo da Construção (INCC):", style = "font-size:90%;font-weight:bold;color:#2C65A1", span("INCC - 10", style = "color:black")),
                                                                                    
                                                                                    
                                                                                    # Estrutura IPA
                                                                                    #                                                                            selectInput("ipa10_estrutura", label = "Estrutura:", width = "50%", 
                                                                                    #                                                                                        choices = c('Selecione uma estrutura' = '', "Estágios de Processamento (EP)", "Origem (OG)")),
                                                                                    #                                                                            # Estágios IPA dependendo da estrutura selecionada acima:
                                                                                    #                                                                            conditionalPanel("input.ipa10_estrutura", 
                                                                                    uiOutput("inccm_estagio"),
                                                                                    conditionalPanel("input.inccm_estagios != 'TODOS OS ESTÁGIOS (MAIOR NÍVEL)'",
                                                                                                     uiOutput("inccm_grupos"),
                                                                                                     conditionalPanel("input.inccm_grupos != 'TODOS OS GRUPOS (MAIOR NÍVEL)'",
                                                                                                                      uiOutput("inccm_subgrupos"),
                                                                                                                      conditionalPanel("input.inccm_subgrupos != 'TODOS OS SUBGRUPOS (MAIOR NÍVEL)'",
                                                                                                                                       uiOutput("inccm_itens")))),
                                                                                    actionButton("visu_inccm", label = "Buscar", icon = icon("search")),
                                                                                    
                                                                                    conditionalPanel("input.visu_inccm",
                                                                                                     bsCollapse(id = "collapse_inccm_resultados", open = 1,
                                                                                                                bsCollapsePanel(value = 1,span("Tabela - INCC-10", style = "font-size:90%;font-weight:bold"), style = "primary",
                                                                                                                                p(tags$b("OBS:"),"Clique na(s) série(s) de interesse para selecioná-la(s) e em seguida clique em", tags$em("Visualizar"),
                                                                                                                                  "para obter mais informações.", style = "color:#2C65A1"),
                                                                                                                                # tabela
                                                                                                                                dataTableOutput("filtro_inccm"),
                                                                                                                                br(),
                                                                                                                                fluidRow(
                                                                                                                                  column(2,
                                                                                                                                         # Botão de visualizar séries
                                                                                                                                         tipify(bsButton("visu_series_inccm", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                                                                "Certifique-se de ter selecionado pelo menos uma série temporal.", placement = "bottom")),
                                                                                                                                  column(2,
                                                                                                                                         # Botão acrescentar séries
                                                                                                                                         tipify(bsButton("add_inccm", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                                                                "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                                                                  )),
                                                                                                                                # Nova janela após clicar no botão
                                                                                                                                bsModal("modal_inccm", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_inccm", size = "large",
                                                                                                                                        
                                                                                                                                        # Conteúdo da nova janela
                                                                                                                                        tabBox(width = "100%", 
                                                                                                                                               tabPanel("Gráfico", dygraphOutput("grafico_inccm")),
                                                                                                                                               tabPanel("Dados", tableOutput("dados_inccm")),
                                                                                                                                               tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_inccm"), style = "font-size:96%")),
                                                                                                                                               tabPanel("Download",
                                                                                                                                                        p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                                                        selectInput("formato_fgv_inccm", div("Formato:", style = "font-size:90%"),
                                                                                                                                                                    choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                                                                    selected = ".csv" ),
                                                                                                                                                        downloadButton('download_fgv_inccm', 'Download')))))))
                                                                                    
                                                                                    
                                                                    )))
                                     ), # FIM INCC
                                     
                                     # # > menu ÍNDICES SETORIAIS ---------------------------------------------------------------------
                                     # navbarMenu(span("Índices Setoriais",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
                                     #            tabPanel("1 - Índices de Obras Hidrelétricas"),
                                     #            tabPanel("2 - Índices de Obras Portuárias"),
                                     #            tabPanel("3 - Índices de Obras Rodoviárias")
                                     # ), # FIM ÍNDICES SETORIAIS
                                     # 
                                     # > menu SONDAGEM ---------------------------------------------------------------------
                                     navbarMenu(span("Sondagem",style = "font-size:95%;font-weight:bold;color:#2C65A1"),
                                                tabPanel("1 - Comércio",
                                                         bsCollapse(id = "collapse_sond_comercio", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Sondagem do Comércio", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                                                    dataTableOutput("tabela_comercio"),
                                                                                    br(),
                                                                                    
                                                                                    fluidRow(
                                                                                      column(2,
                                                                                             # Botão visualizar séries
                                                                                             tipify(bsButton("visu_series_ind", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                    "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                                      column(2,
                                                                                             # Botão acrescentar séries
                                                                                             tipify(bsButton("add_comercio", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                    "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                      )),
                                                                                    # Nova janela após clicar no botão de visualizar séries
                                                                                    bsModal("modal_comercio", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_ind", size = "large",
                                                                                            
                                                                                            # Conteúdo da nova janela
                                                                                            tabBox(width = "100%", 
                                                                                                   tabPanel("Gráfico", dygraphOutput("grafico_comercio"),
                                                                                                            hr(),
                                                                                                            span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                            div(tableOutput("legenda_comercio"), style = "font-size:85%")),                        
                                                                                                   tabPanel("Dados", tableOutput("dados_comercio")),
                                                                                                   tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_comercio"), style = "font-size:96%")),
                                                                                                   tabPanel("Download",
                                                                                                            p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                            selectInput("formato_fgv_comercio", div("Formato:", style = "font-size:90%"),
                                                                                                                        choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                        selected = ".csv" ),
                                                                                                            downloadButton('download_fgv_comercio', 'Download'))
                                                                                                  ))
                                                                    ))),
                                                tabPanel("2 - Construção",
                                                         bsCollapse(id = "collapse_sond_construcao", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Sondagem da Construção", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                                                    dataTableOutput("tabela_construcao"),
                                                                                    br(),
                                                                                    
                                                                                    fluidRow(
                                                                                      column(2,
                                                                                             # Botão visualizar séries
                                                                                             tipify(bsButton("visu_series_cons", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                    "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                                      column(2,
                                                                                             # Botão acrescentar séries
                                                                                             tipify(bsButton("add_construcao", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                    "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                      )),
                                                                                    # Nova janela após clicar no botão de visualizar séries
                                                                                    bsModal("modal_construcao", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_cons", size = "large",
                                                                                            
                                                                                            # Conteúdo da nova janela
                                                                                            tabBox(width = "100%", 
                                                                                                   tabPanel("Gráfico", dygraphOutput("grafico_construcao"),
                                                                                                            hr(),
                                                                                                            span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                            div(tableOutput("legenda_construcao"), style = "font-size:85%")),                        
                                                                                                   tabPanel("Dados", tableOutput("dados_construcao")),
                                                                                                   tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_construcao"), style = "font-size:96%")),
                                                                                                   tabPanel("Download",
                                                                                                            p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                            selectInput("formato_fgv_construcao", div("Formato:", style = "font-size:90%"),
                                                                                                                        choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                        selected = ".csv" ),
                                                                                                            downloadButton('download_fgv_construcao', 'Download'))))
                                                                    ))),
                                                tabPanel("3 - Consumidor",
                                                         bsCollapse(id = "collapse_sond_consumidor", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Sondagem do Consumidor", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                                                    dataTableOutput("tabela_consumidor"),
                                                                                    br(),
                                                                                    
                                                                                    fluidRow(
                                                                                      column(2,
                                                                                             # Botão visualizar séries
                                                                                             tipify(bsButton("visu_series_ec", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                    "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                                      column(2,
                                                                                             # Botão acrescentar séries
                                                                                             tipify(bsButton("add_consumidor", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                    "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                      )),
                                                                                    # Nova janela após clicar no botão de visualizar séries
                                                                                    bsModal("modal_consumidor", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_ec", size = "large",
                                                                                            
                                                                                            # Conteúdo da nova janela
                                                                                            tabBox(width = "100%", 
                                                                                                   tabPanel("Gráfico", dygraphOutput("grafico_consumidor"),
                                                                                                            hr(),
                                                                                                            span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                            div(tableOutput("legenda_consumidor"), style = "font-size:85%")),                        
                                                                                                   tabPanel("Dados", tableOutput("dados_consumidor")),
                                                                                                   tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_consumidor"), style = "font-size:96%")),
                                                                                                   tabPanel("Download",
                                                                                                            p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                            selectInput("formato_fgv_consumidor", div("Formato:", style = "font-size:90%"),
                                                                                                                        choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                        selected = ".csv" ),
                                                                                                            downloadButton('download_fgv_consumidor', 'Download'))))
                                                                    ))),
                                                tabPanel("4 - Indústria",
                                                         bsCollapse(id = "collapse_sond_industria", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Sondagem da Indústria", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                                                    dataTableOutput("tabela_industria"),
                                                                                    br(),
                                                                                    
                                                                                    fluidRow(
                                                                                      column(2,
                                                                                             # Botão visualizar séries
                                                                                             tipify(bsButton("visu_series_com", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                    "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                                      column(2,
                                                                                             # Botão acrescentar séries
                                                                                             tipify(bsButton("add_industria", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                    "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                      )),
                                                                                    # Nova janela após clicar no botão de visualizar séries
                                                                                    bsModal("modal_industria", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_com", size = "large",
                                                                                            
                                                                                            # Conteúdo da nova janela
                                                                                            tabBox(width = "100%", 
                                                                                                   tabPanel("Gráfico", dygraphOutput("grafico_industria"),
                                                                                                            hr(),
                                                                                                            span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                            div(tableOutput("legenda_industria"), style = "font-size:85%")),                        
                                                                                                   tabPanel("Dados", tableOutput("dados_industria")),
                                                                                                   tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_industria"), style = "font-size:96%")),
                                                                                                   tabPanel("Download",
                                                                                                            p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                            selectInput("formato_fgv_industria", div("Formato:", style = "font-size:90%"),
                                                                                                                        choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                        selected = ".csv" ),
                                                                                                            downloadButton('download_fgv_industria', 'Download'))))
                                                                    ))),
                                                tabPanel("5 - Serviços",
                                                         bsCollapse(id = "collapse_sond_servicos", open = 1,
                                                                    bsCollapsePanel(value = 1, style = "default",
                                                                                    title = span("Sondagem de Serviços", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                                                    dataTableOutput("tabela_servico"),
                                                                                    br(),
                                                                                    
                                                                                    fluidRow(
                                                                                      column(2,
                                                                                             # Botão visualizar séries
                                                                                             tipify(bsButton("visu_series_serv", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                    "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                                      column(2,
                                                                                             # Botão acrescentar séries
                                                                                             tipify(bsButton("add_servico", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                    "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                      )),
                                                                                    # Nova janela após clicar no botão de visualizar séries
                                                                                    bsModal("modal_servico", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_serv", size = "large",
                                                                                            
                                                                                            # Conteúdo da nova janela
                                                                                            tabBox(width = "100%", 
                                                                                                   tabPanel("Gráfico", dygraphOutput("grafico_servico"),
                                                                                                            hr(),
                                                                                                            span("Legenda:",style = "font-weight:bold;color:#2C65A1"),
                                                                                                            div(tableOutput("legenda_servico"), style = "font-size:85%")),                        
                                                                                                   tabPanel("Dados", tableOutput("dados_servico")),
                                                                                                   tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_servico"), style = "font-size:96%")),
                                                                                                   tabPanel("Download",
                                                                                                            p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                                                            selectInput("formato_fgv_servico", div("Formato:", style = "font-size:90%"),
                                                                                                                        choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                        selected = ".csv" ),
                                                                                                            downloadButton('download_fgv_servico', 'Download'))))
                                                                    )))
                                     ) # FIM SONDAGEM 
                                     
                          ) # fim navbarPage
                  ), # fim Banco de ST's FGV | IBRE
                                     
                  # Item: Banco de ST's -> BRASIL ---------------------------------------------------------------------------------------- 
                  tabItem(tabName = "st_brasil",
                          bsCollapse(id = "collapse_stbrasil", open = 1, 
                                     bsCollapsePanel(value = 1, style = "default",
                                                     title = span(icon('long-arrow-right'), tags$b("Brasil"), style = "font-size:90%;color:#2C65A1"),
                                                     div("Navegue pelas abas abaixo para encontrar as séries de interesse."),
                                                     div("A aba", tags$em("Brasil"), "contém uma lista com todas as séries disponíveis.",
                                                         "Nas outras abas, as séries estão divididas por fonte."),
                                                     p(tags$b("OBS:"),"Clique na série de interesse para selecioná-la e em seguida clique em", tags$em("Visualizar"),
                                                       "para obter mais informações.", style = "color:#2C65A1"),
                                                     hr(),
                                                     fluidRow(
                                                       
                                                       # Visualizar as séries disponíveis
                                                       tabBox(width = 12, id = "tab_mundo",
                                                              # Séries Disponíveis
#                                                               tabPanel("Brasil",
#                                                                        "..."),
                                                              tabPanel("IBGE",
                                                                       dataTableOutput("tabela_IBGE"),
                                                                       #verbatimTextOutput("ingrid"),
                                                                       
                                                                    # textOutput("teste_erro"),
                                                                    conditionalPanel("output.liberaibge==0",
                                                                       fluidRow(
                                                                         column(2,
                                                                                # Botão visualizar séries
                                                                                tipify(bsButton("visu_series_IBGE", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                       "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                         column(2,
                                                                                # Botão acrescentar séries
                                                                                tipify(bsButton("add_IBGE", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                       "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")))),
                                                          
                                                                      textOutput("teste_erro"),
                                                                      # tableOutput("pirint"),
                                                                      bsAlert("alert"),
   
                                                                         
#                                                                          conditionalPanel("output.teste_erro!=0", 
# #                                                                                         h3("Escolha séries com a mesma periodicidade!!",style = "color:#f91818")),
#                                                                             tabPanel("erro",h3("Escolha séries com a mesma periodicidade!!",style = "color:#f91818"))),
                                                                          
                                                  
                                                                         # conditionalPanel("output.teste_erro==0", 
                                                                                          bsModal("modal_IBGE", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_IBGE", size = "large",
                                                                                                                               
                                                                                                                               # Conteúdo da nova janela
                                                                                                                               tabBox(width = "100%", 
                                                                                                                                       tabPanel("Gráfico", dygraphOutput("grafico")),
                                                                                                                                      tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_stIBGE"), style = "font-size:96%")),
                                                                                                                                        tabPanel("Dados",tableOutput("pirint")),
                                                                                                                                      tabPanel("Download",
                                                                                                                                        p("Exportar série temporal:", style = "font-weight:bold;color:#2C65A1"),
                                                                                                                                        selectInput("formato_IBGE", div("Formato:", style = "font-size:90%"),
                                                                                                                                                    choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                                                                    selected = ".csv" ),
                                                                                                                                        downloadButton('download_IBGE', 'Download'))))
                                                                         
                                                                        
                                                                      
                                                                         
                                                                        ),
                                                              tabPanel("Banco Central",
                                                                       uiOutput("bacen_grupo"),
                                                                       
                                                                       conditionalPanel("input.bacen_grupos != 'TODOS OS GRUPOS'",
                                                                                        uiOutput("bacen_subgrupo")),
                                                                       
                                                                       
                                                                       conditionalPanel("input.bacen_subgrupo != 'TODOS OS SUBGRUPOS'",
                                                                                        dataTableOutput("bacentabela")
                                                                                        # tableOutput("dados_bacen")
                                                                                        ),  
                                                                       conditionalPanel("output.liberabacen==0",fluidRow(column(2,
                                                                                                 # Botão visualizar séries
                                                                                                 tipify(bsButton("visu_series_bacen", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                        "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom")),
                                                                                          column(2,
                                                                                                 # Botão acrescentar séries
                                                                                                 tipify(bsButton("add_bacen", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                                                        "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom")
                                                                                          ))),
                                                                       
                                                                       bsModal("modal_bacen", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_bacen", size = "large",
                                                                               
                                                                               # Conteúdo da nova janela
                                                                               tabBox(width = "100%", 
                                                                                      tabPanel("Gráfico", dygraphOutput("grafico_bacen")),
                                                                              
                                                                                      tabPanel("Dados",tableOutput("dados_bacen")),
                                                                                      tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_bacen"), style = "font-size:96%")),
                                                                                      tabPanel("Download",
                                                                                               p("Exportar série temporal:", style = "font-weight:bold;color:#2C65A1"),
                                                                                               selectInput("formato_bacen", div("Formato:", style = "font-size:90%"),
                                                                                                           choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                           selected = ".csv" ),
                                                                                               downloadButton('download_BACEN', 'Download')
                                                                                               
                                                                                               #verbatimTextOutput("teste_bacen")
                                                                                               )
                                                                       
                                                                       
                                                                       ))
#                                                               tabPanel("Câmbio",
#                                                                        div(tags$b("Dólar/Real"),style = "color:#2C65A1"),
#                                                                        div(tags$b("Última atualização:"), textOutput("brasil_cambio_data")),
#                                                                        hr(),
#                                                                        fluidRow(
#                                                                          
#                                                                        column(1, div(tags$em("Close:"), textOutput("brasil_cambio_Close"))),
#                                                                        column(1,div(tags$em("Open:"), textOutput("brasil_cambio_Open"))),
#                                                                        column(1,div(tags$em("High:"), textOutput("brasil_cambio_High"))),
#                                                                        column(1,div(tags$em("Low:"), textOutput("brasil_cambio_Low")))
#                                                                        ),
#                                                                        div(dygraphOutput("brasil_cambio_grafico", width = "85%"), style="align:center")
#                                                                        )
                                                       )
                                                     
                                                     
                                                     
                                                     
                                     ))))
                  ),
                  # Item: Banco de ST's -> AMÉRICA LATINA ---------------------------------------------------------------------------------------- 
                  tabItem(tabName = "st_america",
                          bsCollapse(id = "collapse_stamerica", open = 1, 
                                     bsCollapsePanel(value = 1, style = "default",
                                                     title = span(icon('long-arrow-right'), tags$b("América Latina"), style = "font-size:90%;color:#2C65A1"),
                                                     p("Navegue pelas abas abaixo para encontrar as séries disponíveis."),
                                                     p(tags$b("OBS:"),"Clique na série de interesse para selecioná-la e em seguida clique em", tags$em("Visualizar"),
                                                       "para obter mais informações.", style = "color:#2C65A1"),
                                                     hr(),
                                                     "Em construção"
                                     ))
                  ),
                  # Item: Banco de ST's -> MUNDO ---------------------------------------------------------------------------------------- 
                  tabItem(tabName = "st_mundo",
                          bsCollapse(id = "collapse_stmundo", open = 1, 
                                     bsCollapsePanel(value = 1, style = "default",
                                                     title = span(icon('long-arrow-right'), tags$b("Mundo"), style = "font-size:90%;color:#2C65A1"),
                                                     div("Navegue pelas abas abaixo para encontrar as séries de interesse."),
                                                     div("A aba", tags$em("Mundo"), "contém uma lista com todas as séries disponíveis.",
                                                       "Nas outras abas, as séries estão divididas em grupos."),
                                                     p(tags$b("OBS:"),"Clique na série de interesse para selecioná-la e em seguida clique em", tags$em("Visualizar"),
                                                       "para obter mais informações.", style = "color:#2C65A1"),
                                                     hr(),
                                                     fluidRow(
                                                       
                                                       # Visualizar as séries disponíveis
                                                       tabBox(width = 12, id = "tab_mundo",
                                                              # Séries Disponíveis
                                                              tabPanel("Mundo",
                                                                       dataTableOutput("tabela_mundo"),
                                                                       tipify(bsButton("visu_stmundo", "Visualizar", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                              "Certifique-se de ter selecionado uma série temporal.",  placement = "bottom"),
                                                                       # Botão acrescentar séries
                                                                       tipify(bsButton("add_mundo", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                              "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom"),
                                                                      
                                                                       # Nova janela após clicar no botão de visualizar
                                                                       bsModal("modal_stmundo", title = div("Informações", style = "font-weight:bold"), trigger = "visu_stmundo", size = "large",
                                                                               
                                                                               # Conteúdo da nova janela
                                                                               tabBox(width = "100%", 
                                                                                      tabPanel("Gráfico", dygraphOutput("grafico_stmundo")),                        
                                                                                      tabPanel("Dados", tableOutput("dados_stmundo")),
                                                                                      tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_stmundo"), style = "font-size:96%")),
                                                                                      tabPanel("Download",
                                                                                               p("Exportar série temporal:", style = "font-weight:bold;color:#2C65A1"),
                                                                                               selectInput("formato_stmundo", div("Formato:", style = "font-size:90%"),
                                                                                                           choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                           selected = ".csv" ),
                                                                                               downloadButton('download_stmundo', 'Download'))
                                                                               )
                                                                       )                                      
                                                                       ),
                                                              
                                                              tabPanel("EUA",
                                                                       dataTableOutput("tabela_mundo_eua"),
                                                                       tipify(bsButton("visu_stmundo_eua", "Visualizar", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                              "Certifique-se de ter selecionado uma série temporal.",  placement = "bottom"),
                                                                       tipify(bsButton("add_mundo_eua", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                              "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom"),
                                                                       
                                                                       # Nova janela após clicar no botão de visualizar
                                                                       bsModal("modal_stmundo_eua", title = div("Informações", style = "font-weight:bold"), trigger = "visu_stmundo_eua", size = "large",
                                                                               
                                                                               # Conteúdo da nova janela
                                                                               tabBox(width = "100%",
                                                                                      tabPanel("Gráfico", dygraphOutput("grafico_stmundo_eua")),                        
                                                                                      tabPanel("Dados", tableOutput("dados_stmundo_eua")),
                                                                                      tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_stmundo_eua"), style = "font-size:96%")),
                                                                                      tabPanel("Download",
                                                                                               p("Exportar série temporal:", style = "font-weight:bold;color:#2C65A1"),
                                                                                               selectInput("formato_stmundo_eua", div("Formato:", style = "font-size:90%"),
                                                                                                           choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                           selected = ".csv" ),
                                                                                               downloadButton('download_stmundo_eua', 'Download'))
                                                                               )
                                                                       )),
                                                              tabPanel("América Latina",
                                                                       dataTableOutput("tabela_mundo_al"),
                                                                       tipify(bsButton("visu_stmundo_al", "Visualizar", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                                                                 "Certifique-se de ter selecionado uma série temporal.",  placement = "bottom"),
                                                                       tipify(bsButton("add_mundo_al", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                              "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom"),
                                                                       # Nova janela após clicar no botão de visualizar
                                                                       bsModal("modal_stmundo_al", title = div("Informações", style = "font-weight:bold"), trigger = "visu_stmundo_al", size = "large",
                                                                               
                                                                               # Conteúdo da nova janela
                                                                               tabBox(width = "100%",
                                                                                      tabPanel("Gráfico", dygraphOutput("grafico_stmundo_al")),
                                                                                      tabPanel("Dados", tableOutput("dados_stmundo_al")),
                                                                                      tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_stmundo_al"), style = "font-size:96%")),
                                                                                      tabPanel("Download",
                                                                                               p("Exportar série temporal:", style = "font-weight:bold;color:#2C65A1"),
                                                                                               selectInput("formato_stmundo_al", div("Formato:", style = "font-size:90%"),
                                                                                                           choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                           selected = ".csv" ),
                                                                                               downloadButton('download_stmundo_al', 'Download'))))),
                                                              tabPanel("Europa",
                                                                        dataTableOutput("tabela_mundo_europa"),
                                                                       #verbatimTextOutput("linhaseuropa"),
                                                                        tipify(bsButton("visu_stmundo_europa", "Visualizar", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                               "Certifique-se de ter selecionado uma série temporal.",  placement = "bottom"),
                                                                       tipify(bsButton("add_mundo_europa", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                              "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom"),
                                                                        
                                                                        # Nova janela após clicar no botão de visualizar
                                                                        bsModal("modal_stmundo_europa", title = div("Informações", style = "font-weight:bold"), trigger = "visu_stmundo_europa", size = "large",
                                                                                
                                                                                # Conteúdo da nova janela
                                                                                tabBox(width = "100%",
                                                                                       tabPanel("Gráfico", dygraphOutput("grafico_stmundo_europa")),                        
                                                                                       tabPanel("Dados", tableOutput("dados_stmundo_europa")),
                                                                                       tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_stmundo_europa"), style = "font-size:96%")),
                                                                                       tabPanel("Download",
                                                                                                p("Exportar série temporal:", style = "font-weight:bold;color:#2C65A1"),
                                                                                                selectInput("formato_stmundo_europa", div("Formato:", style = "font-size:90%"),
                                                                                                            choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                            selected = ".csv" ),
                                                                                                downloadButton('download_stmundo_europa', 'Download'))
                                                                                )
                                                                        )),
                                                              tabPanel("Ásia",
                                                                       dataTableOutput("tabela_mundo_asia"),
                                                                       #verbatimTextOutput("linhasasia"),
                                                                       tipify(bsButton("visu_stmundo_asia", "Visualizar", icon = icon("line-chart"), size = "small", style = "primary"),
                                                                              "Certifique-se de ter selecionado uma série temporal.",  placement = "bottom"),
                                                                       tipify(bsButton("add_mundo_asia", label = "Marcar como favorita", icon = icon("cube"), size = "small"),
                                                                              "Adicione uma ou mais séries temporais para consulta.",  placement = "bottom"),
                                                                       # Nova janela após clicar no botão de visualizar
                                                                       bsModal("modal_stmundo_asia", title = div("Informações", style = "font-weight:bold"), trigger = "visu_stmundo_asia", size = "large",
                                                                               
                                                                               # Conteúdo da nova janela
                                                                               tabBox(width = "100%",
                                                                                      tabPanel("Gráfico", dygraphOutput("grafico_stmundo_asia")),                        
                                                                                      tabPanel("Dados", tableOutput("dados_stmundo_asia")),
                                                                                      tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_stmundo_asia"), style = "font-size:96%")),
                                                                                      tabPanel("Download",
                                                                                               p("Exportar série temporal:", style = "font-weight:bold;color:#2C65A1"),
                                                                                               selectInput("formato_stmundo_asia", div("Formato:", style = "font-size:90%"),
                                                                                                           choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                           selected = ".csv" ),
                                                                                               downloadButton('download_stmundo_asia', 'Download'))
                                                                               )
                                                                       )),
                                                              tabPanel("Câmbio",
                                                                       selectInput("mundo_cambio", "Moeda", choices = c("Escolha a moeda"='', 
                                                                                                                        "Real (Brasil)",
                                                                                                                        "Boliviano (Bolivia)",
                                                                                                                        "Euro (Euro)",
                                                                                                                        "Guarani (Paraguai)",
                                                                                                                        "Novo Peso (Uruguai)",
                                                                                                                        "Peso (Argentina)",
                                                                                                                        "Peso (Chile)",
                                                                                                                        "Peso (Colombia)",
                                                                                                                        "Peso (Cuba)",
                                                                                                                        "Peso (Mexico)",
                                                                                                                        "Yen (Japao)",
                                                                                                                        "Yuan (China)")),
                                                                       div("Fonte:", a(tags$em("Yahoo! Finance"),
                                                                                            href="http://finance.yahoo.com/;_ylt=AjBOPGNyURli1QPOL4vze0N.FJF4",
                                                                                       style = "color:#2C65A1;font-size:94%"),
                                                                           style = "font-size:94%"),
                                                                       br(),
                                                                       conditionalPanel("input.mundo_cambio",
                                                                                      
                                                                       div(tags$b("Última atualização:"), textOutput("mundo_cambio_data")),
                                                                       
                                                                       hr(),
                                                                       fluidRow(
                                                                         
                                                                         column(1, div(tags$em("Close:"), textOutput("mundo_cambio_Close"))),
                                                                         column(1,div(tags$em("Open:"), textOutput("mundo_cambio_Open"))),
                                                                         column(1,div(tags$em("High:"), textOutput("mundo_cambio_High"))),
                                                                         column(1,div(tags$em("Low:"), textOutput("mundo_cambio_Low")))
                                                                       ),
                                                                       hr(),
                                                                       div(dygraphOutput("mundo_cambio_grafico", width = "85%"), style="align:center")
                                                                       )
                                                              ),
                                                              tabPanel("Indicadores Sociais",
                                                                       selectInput("mundo_indic", "Indicadores Sociais:", choices = c("Escolha o indicador"='',
                                                                                                                       "Esperança de Vida ao Nascer",
                                                                                                                       "IDH (Índice de Desenvolvimento Humano)",
                                                                                                                       "Índice de Gini",
                                                                                                                       "População",
                                                                                                                       "Taxa de Analfabetismo",
                                                                                                                       "Taxa de Mortalidade Infantil")
                                                                                                                       ),
                                                                       conditionalPanel("input.mundo_indic == 'Índice de Gini'",
                                                                                        htmlOutput("indic_gini"),
                                                                                        div(tags$b("Última atualização:"), "2010", style = "color:#2C65A1"),
                                                                                        div(tags$b("Fonte:"), a("World Bank", href = "http://data.worldbank.org/"),
                                                                                            style = "color:#2C65A1")),
                                                                       conditionalPanel("input.mundo_indic == 'Esperança de Vida ao Nascer'",
                                                                                        htmlOutput("indic_expec"),
                                                                                        div(tags$b("Última atualização:"), "2012", style = "color:#2C65A1"),
                                                                                        div(tags$b("Fonte:"), a("World Bank", href = "http://data.worldbank.org/"),
                                                                                            style = "color:#2C65A1")),
                                                                       conditionalPanel("input.mundo_indic == 'Taxa de Mortalidade Infantil'",
                                                                                        htmlOutput("indic_tmi"),
                                                                                        div(tags$b("Última atualização:"), "2010", style = "color:#2C65A1"),
                                                                                        div(tags$b("Fonte:"), a("World Bank", href = "http://data.worldbank.org/"),
                                                                                            style = "color:#2C65A1")),
                                                                       conditionalPanel("input.mundo_indic == 'População'",
                                                                                        htmlOutput("indic_pop"),
                                                                                        div(tags$b("Última atualização:"), "2013", style = "color:#2C65A1"),
                                                                                        div(tags$b("Fonte:"), a("World Bank", href = "http://data.worldbank.org/"),
                                                                                            style = "color:#2C65A1")))
                                                            
                                                       ))
                                     ))
                  ),
                  # Item: Banco de ST's -> FAVORITAS ---------------------------------------------------------------------------------------- 
tabItem(tabName = "st_favoritas",
        #tableOutput("testeperiodicidade"),
        dataTableOutput("favoritos"),
        #tableOutput("dados_incc"),
        
      #verbatimTextOutput("linhas"),
        
      
        
        fluidRow(
          column(2,
                 # Botão visualizar séries
                 
                 conditionalPanel("output.libera ==0", tipify(bsButton("visu_series_favoritas", "Visualizar Séries", icon = icon("line-chart"), size = "small", style = "primary"),
                        "Certifique-se de ter selecionado pelo menos uma série temporal.",  placement = "bottom"))),
                 # column(2,downloadLink('download_favoritos', 'Download binary'))
                       # Botão acrescentar séries
                        tipify(bsButton("Save", label = "Salvar Favoritas", icon = icon("save"), size = "small"),
                               "Salve suas séries favoritas.",  placement = "bottom")
                  
        ),
        
          #                         verbatimTextOutput("problema"),

        conditionalPanel("output.libera ==0",
                         bsModal("modal_favoritos", title = div("Resultados", style = "font-weight:bold"), trigger = "visu_series_favoritas", size = "large",

                                                     # Conteúdo da nova janela
                                                     tabBox(width = "100%",
                                                            tabPanel("Gráfico", dygraphOutput("grafico_favoritos"),
                                                                     hr(),
                                                                     span("Legenda:",style = "font-weight:bold;color:#2C65A1")),
                                                            #                                                   div(tableOutput("legenda_igp"), style = "font-size:85%")),
                                                            tabPanel("Dados", tableOutput("dados_favoritos")),
                                                            tabPanel("Estatísticas Descritivas", div(tableOutput("descritiva_favoritos"), style = "font-size:96%")),
                                                            tabPanel("Download",
                                                                     p("Exportar série(s) temporal(is):", style = "font-weight:bold;color:#2C65A1"),
                                                                     selectInput("formato_fgv_favoritos", div("Formato:", style = "font-size:90%"),
                                                                                 choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                 selected = ".csv" ),
                                                                     downloadButton('download_fgv_favoritos', 'Download'))
                                                     )))


        # )

        #                           conditionalPanel("output.problema=='Escolha séries com a mesma periodicidade'",
        #
        #                                            verbatimTextOutput("problema")
        #
        #                                            )
        
),
                  # Item: Ajuste Sazonal -------------------------------------------------------------------------------------------
                  tabItem(tabName = "ajuste_sazonal",
                          bsCollapse(id = "collapse_ajustesazonal", open = 1,
                                     bsCollapsePanel(value = 1, style = "default",
                                                     title = span(icon("line-chart"),"Ajuste Sazonal", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                     
                                                     div("Aqui você pode dessazonalizar uma série temporal com o X13-ARIMA-SEATS (U.S. Census Bureau)."),
                                                     p("Para saber mais sobre a metodologia empregada, acessa a opção", tags$i("Saiba mais"),"."),
                                                     
                                                     p("Selecione alguma opção disponível na aba Exemplos ou importe a sua própria série temporal."),
                                                     
                                                     hr(),
                                                     fluidRow(
                                                       
                                                       # Visualizar as séries disponíveis
                                                       tabBox(width = 3, id = "tab_sazon",
                                                              # Séries Disponíveis
                                                              tabPanel("Exemplos",
                                                                       selectInput("sazon", p("Séries Temporais (ST) disponíveis:", style = "font-size:96%"),
                                                                                   choices = c(list("Clássicas" = c("Airline","UK Driver Deaths")),
                                                                                               list("Séries Econômicas" = c("IPC-DI (IBRE)", "PIM-PF (IBGE)"))),
                                                                                   selected = "Airline"),
                                                                       hr(),
                                                                       
                                                                       # Personalizar ajuste sazonal
                                                                       conditionalPanel("input.tab_sazon == 'Exemplos'",
                                                                                        strong("Personalizar ajuste:", style = "color:steelblue"),
                                                                                        br(),
                                                                                        br(),
                                                                                        selectInput("metodo_ajuste", p("Método de ajuste:", style = "font-size:96%"),
                                                                                                    choices = c("SEATS", "X11"), multiple = FALSE, selected = "SEATS"),
                                                                                        selectInput("transform", p("Usar transformação?", style = "font-size:96%"),
                                                                                                    choices = c("Automático","Log", "Raiz Quadrada", "Inversa", "Nenhuma"), multiple = FALSE, selected = "Automático"),
                                                                                        selectInput("outlier", p("Detectar Outlier?", style = "font-size:96%"),
                                                                                                    choices = c("Sim", "Não"), multiple = FALSE, selected = "Sim"),
                                                                                        selectInput("reg.aictest", p("Variáveis de regressão?", style = "font-size:96%"),
                                                                                                    choices = c("Sim", "Não"), multiple = FALSE, selected = "Sim")
                                                                                        
                                                                       )),
                                                              
                                                              tabPanel("Importar",
                                                                       tipify(fileInput('arquivo_local_as', label = p("Importar arquivo .csv:", style = "font-size:96%"),multiple = FALSE, accept = '.csv'),
                                                                              "Apenas arquivos em formato .csv",
                                                                              placement = "top", trigger = "hover"),
                                                                       
                                                                       conditionalPanel("!output.arquivo_carregado_as",
                                                                                        p(tags$b("OBS1:"),"Funciona apenas para séries temporais mensais.", style = "font-size:90%; color:#2C65A1"),
                                                                                        p(tags$b("OBS2:"),"Para importar a sua série temporal, crie um arquivo", tags$b(".csv"), "com as seguintes colunas.",
                                                                                          style = "font-size:90%; color:#2C65A1"),
                                                                                        img(src="importar.png")
                                                                       ),
                                                                       hr(),
                                                                       # Personalizar ajuste sazonal
                                                                       conditionalPanel("input.tab_sazon == 'Importar' & output.arquivo_carregado_as",
                                                                                        strong("Personalizar ajuste:", style = "color:steelblue"),
                                                                                        br(),
                                                                                        br(),
                                                                                        selectInput("metodo_ajuste_imp", p("Método de ajuste:", style = "font-size:96%"),
                                                                                                    choices = c("SEATS", "X11"), multiple = FALSE, selected = "SEATS"),
                                                                                        selectInput("transform_imp", p("Usar transformação?", style = "font-size:96%"),
                                                                                                    choices = c("Automático","Log", "Raiz Quadrada", "Inversa", "Nenhuma"), multiple = FALSE, selected = "Automático"),
                                                                                        selectInput("outlier_imp", p("Detectar Outlier?", style = "font-size:96%"),
                                                                                                    choices = c("Sim", "Não"), multiple = FALSE, selected = "Sim"),
                                                                                        selectInput("reg.aictest_imp", p("Variáveis de regressão?", style = "font-size:96%"),
                                                                                                    choices = c("Sim", "Não"), multiple = FALSE, selected = "Sim")
                                                                                        
                                                                       ))
                                                              
                                                              
                                                       ),
                                                       
                                                       # Resultados do ajuste sazonal pras séries disponíveis
                                                       conditionalPanel("input.tab_sazon == 'Exemplos'",
                                                                        tabBox( width = 9,
                                                                                
                                                                                tabPanel("Gráfico",
                                                                                         dygraphOutput("ajuste_graf_disp")),
                                                                                tabPanel("Diagnósticos",
                                                                                         p("Significância de parâmetros, critérios de informação e testes de autocorrelação e normalidade:", style = "color:dimgrey;font-size:98%", align = "center"),
                                                                                         verbatimTextOutput("ajuste_summary"),
                                                                                         hr(),
                                                                                         div("Verificação de Sazonalidade:", style = "color:dimgrey;font-size:98%", align = "center"),
                                                                                         p("H0: Há sazonalidade na série temporal.", style = "color:dimgrey;font-size:95%", align = "center"),
                                                                                         verbatimTextOutput("ajuste_qs")),
                                                                                tabPanel("Dados", tableOutput("tabela_disp")),
                                                                                tabPanel("Previsão",
                                                                                         #div("Previsão da série temporal selecionada com ajuste sazonal"),
                                                                                         dygraphOutput("ajuste_prev_st_disp"),
                                                                                         hr(),
                                                                                         dygraphOutput("ajuste_prev_fatores_disp")),
                                                                                tabPanel("Exportar Resultados",
                                                                                         p("Os resultados disponíveis para download são as informações dispostas na aba 'Dados'
                                                                                           que contém os dados originais, os dados com ajuste sazonal, as componentes tendência,
                                                                                           sazonal e irregular, e também a previsão dos fatores sazonais para os próximos 12 meses."),
                                                                                         selectInput("formato_as", div("Selecione o formato do arquivo:", style = "font-size:96%"),
                                                                                                     choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                     selected = ".csv"),
                                                                                         downloadButton('download_as', 'Download'))
                                                                                )),
                                                       
                                                       
                                                       # Resultados do ajuste sazonal pro arquivo input
                                                       conditionalPanel("output.arquivo_carregado_as & input.tab_sazon == 'Importar'",
                                                                        tabBox( width = 9,
                                                                                tabPanel("Gráfico",
                                                                                         dygraphOutput("ajuste_graf_imp")),
                                                                                tabPanel("Diagnósticos",
                                                                                         p("Significância de parâmetros, critérios de informação e testes de autocorrelação e normalidade:", style = "color:dimgrey;font-size:98%", align = "center"),
                                                                                         verbatimTextOutput("ajuste_imp_summary"),
                                                                                         hr(),
                                                                                         div("Verificação de Sazonalidade:", style = "color:dimgrey;font-size:98%", align = "center"),
                                                                                         p("H0: Há sazonalidade na série temporal.", style = "color:dimgrey;font-size:95%", align = "center"),
                                                                                         verbatimTextOutput("ajuste_imp_qs")),
                                                                                tabPanel("Dados", tableOutput("tabela_imp")),
                                                                                tabPanel("Previsão",
                                                                                         #div("Previsão da série temporal selecionada com ajuste sazonal"),
                                                                                         dygraphOutput("ajuste_prev_st_imp"),
                                                                                         hr(),
                                                                                         dygraphOutput("ajuste_prev_fatores_imp")),
                                                                                tabPanel("Exportar Resultados",
                                                                                         p("Os resultados disponíveis para download são as informações dispostas na aba 'Dados'
                                                                que contém os dados originais, os dados com ajuste sazonal, as componentes tendência,
                                                                sazonal e irregular, e também a previsão dos fatores sazonais para os próximos 12 meses."),
                                                                                         selectInput("formato_as_imp", div("Selecione o formato do arquivo:", style = "font-size:96%"),
                                                                                                     choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                     selected = ".csv"),
                                                                                         downloadButton('download_as_imp', 'Download'))))
                                                       
                                                       
                                                     )
                                     )
                          )),
                  
                  # Item: Modelo Paramétrico ------------------------------------------------------------------------------------------- 
                  # tabItem(tabName = "parametrico", 
                  #         
                  #         bsCollapse(id = "collapse_parametrico", open = 1,
                  #                    bsCollapsePanel(value = 1, style = "default",
                  #                                    title = span(icon("exchange"),"Modelo Paramétrico", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                  #                                    
                  #         div("Aqui você é capaz de ajustar uma regressão linear usando sua série de interesse com as séries da FGV Dados como covariáveis.
                  #             Na aba Pesos, você pode fixar os coeficientes para cada covariável, criando seu próprio índice paramétrico."),
                  #         hr(),
                  #         
                  #         # Importar arquivo .xlsx local
                  #         tipify(fileInput('arquivo_local_mp', label = p("Importar arquivo .xlsx:", style = "font-size:96%"),multiple = FALSE, accept = '.xlsx'),
                  #                "Apenas arquivos em formato .xlsx com duas ou mais colunas com títulos. Coluna 1: data (dd/mm/aaaa), coluna 2: Série alvo e colunas seguintes: covariáveis.",
                  #                placement = "top", trigger = "hover"),
                  #         div("Funciona apenas para séries mensais por enquanto.", style = "color:dimgray; font-size:90%"),
                  #         
                  #         box(  width = 3, status = "warning", title = strong("Modelos", style = "font-size:90%"),
                  #               conditionalPanel("output.arquivo_carregado_mp",
                  #                                tabBox(width = "100%", id = "tab_auto",
                  #                                       tabPanel("Automático",
                  #                                                # Pesos modelados
                  #                                                numericInput("defx", div("Máx. def. X:", style = "font-size:96%"), min = 0, max = 5, value = 0),
                  #                                                numericInput("defy", div("Máx. def. Y:", style = "font-size:96%"), min = 0, max = 5, value = 0),
                  #                                                uiOutput("select_var"),
                  #                                                radioButtons("auto", p("Seleção automática?",style = "font-size:96%"), choices = c("Sim", "Não")),
                  #                                                actionButton("atualizar","Atualizar"),
                  #                                                hr(),
                  #                                                conditionalPanel("output.arquivo_carregado_mp",
                  #                                                                 selectInput("formato_mp", div("Exportar Resultados:", style = "font-size:96%"),
                  #                                                                             choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                  #                                                                             selected = ".csv"),
                  #                                                                 downloadButton('download_mp', 'Download'))
                  #                                       ),
                  #                                       tabPanel("Pesos",
                  #                                                # Pesos escolhidos pelo usuário
                  #                                                uiOutput("select_var_pesos"),
                  #                                                textInput("pesos", label = div("Insira os respectivos pesos:", style = "font-size:96%"), value = 0),
                  #                                                verbatimTextOutput("pesos"),
                  #                                                
                  #                                                actionButton("atualizar_pesos","Atualizar")
                  #                                       )
                  #                                       
                  #                                       
                  #                                ))),
                  #         
                  #         
                  #         box(status = "primary", width = 9, title = strong("Resultados", style = "font-size:90%"),
                  #             conditionalPanel("output.arquivo_carregado_mp", 
                  #                              conditionalPanel( "input.tab_auto == 'Automático'", 
                  #                                                conditionalPanel("input.atualizar",
                  #                                                                 tabBox( width = "100%",
                  #                                                                         tabPanel("Gráfico", dygraphOutput("mp_grafico")),
                  #                                                                         tabPanel("Especificações do ajuste", verbatimTextOutput("summary_mp")),
                  #                                                                         tabPanel("Análise dos Resíduos", dygraphOutput("mp_grafico_res"))))),
                  #                              conditionalPanel( "input.tab_auto == 'Pesos'", 
                  #                                                conditionalPanel("input.atualizar_pesos",
                  #                                                                 tabBox( width = "100%",
                  #                                                                         tabPanel("Gráfico", dygraphOutput("mp_grafico_pesos")))))
                  #                              
                  #             )
                  #         )
                  #                    ))
                  #         
                  #         
                  #         ),
tabItem(tabName = "parametrico", 
        
        bsCollapse(id = "collapse_parametrico", open = 1,
                   bsCollapsePanel(value = 1, style = "default",
                                   title = span(icon("exchange"),"Modelo Paramétrico", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                   
                                   div("Esta seção permite que você insira a série de interesse e estime um modelo paramétrico automático. Além disso, é possível
                                       escolher entre as séries econômicas disponíveis no Smart IBRE aquelas que, intuitivamente, melhor 
                                       expliquem a série de interesse."),
                                   hr(),
                                   
                                   box(  width = 4, status = "warning", title = strong("Especificações do Modelo Paramétrico", style = "font-size:90%"),
                                         
                                         # Importar arquivo .xlsx local
                                         tipify(fileInput('arquivo_local_mp2', label = p("Insira a série de interesse:", style = "font-size:96%"), multiple = FALSE, accept = '.xlsx'),
                                                "Apenas arquivos em formato .xlsx com duas colunas com títulos. Coluna 1: data (dd/mm/aaaa), coluna 2: Série alvo.",
                                                placement = "top", trigger = "hover"),
                                         
                                         # Esse painel aparece quando o arquivo é carregado
                                         conditionalPanel("output.arquivo_carregado_mp2",
                                                          tabPanel("Automático",
                                                                   hr(),
                                                                   tipify(textInput("cov_max", label = div("Máximo de variáveis explicativas permitido no modelo:", style = "font-size:96%"), value = 0),
                                                                          "Atenção! O tempo de execução cresce diretamente com o aumento do número de variáveis explicativas.",
                                                                          placement = "top",trigger = "hover"),
                                                                   radioButtons("variacao", p("A variável de interesse corresponde à variação mensal?",style = "font-size:96%"), choices = c("Sim", "Não"), inline = TRUE, selected = "Sim"),
                                                                   hr(),
                                                                   div("Selecione as potenciais variáveis explicativas do modelo:", style = 'font-weight: bold; font-size:96%'),
                                                                   
                                                                   # "Esconde" a lista de índices quando a selecionado "Todas as séries"
                                                                   conditionalPanel(
                                                                     condition = "input.todas_as_series == false & input.carrinho == false",
                                                                     checkboxInput("incc", label = "Índice Nacional de Custo da Construção"),
                                                                     
                                                                     # Esse painel aparece caso INCC seja selecionado
                                                                     conditionalPanel(
                                                                       condition = "input.incc == true",
                                                                       div(uiOutput("choose_incc"),
                                                                           style =  "padding-left: 25px; margin-top:-25px"),
                                                                       conditionalPanel(
                                                                         condition = "output.nav == 'sim' || input.grupos_incc == 'inccgrupo2'",
                                                                         div(uiOutput("choose_incc2"),
                                                                             style =  "padding-left: 50px; margin-top:-25px")
                                                                       )
                                                                     ),
                                                                     checkboxInput("ipa", "Índice de Preços por Atacado"),
                                                                     
                                                                     # Esse painel aparece caso IPA seja selecionado
                                                                     conditionalPanel(
                                                                       condition = "input.ipa == true",
                                                                       div(uiOutput("choose_ipa"),
                                                                           style =  "padding-left: 25px; margin-top:-25px")
                                                                     ),
                                                                     checkboxInput("ipc", "Índice de Preços ao Consumidor"),
                                                                     
                                                                     # Esse painel aparece caso IPC seja selecionado
                                                                     conditionalPanel(
                                                                       condition = "input.ipc == true",
                                                                       div(uiOutput("choose_ipc"),
                                                                           style =  "padding-left: 25px; margin-top:-25px")
                                                                     )),
                                                                   conditionalPanel(
                                                                     condition = "input.todas_as_series == false",
                                                                     tipify(checkboxInput("carrinho", label = "Favoritos"),
                                                                            "Séries escolhidas na aba (...)",
                                                                            placement = "left", trigger = "hover")),
                                                                   conditionalPanel(
                                                                     condition = "input.carrinho == false",
                                                                     checkboxInput("todas_as_series", label = "Todas as séries")),
                                                                   hr(),
                                                                   # div(actionButton("atualizar2","Atualizar"), style = 'weight: 10px'),
                                                                   div(actionButton("atualizar2","Índice Paramétrico", width = '145px'), style="float:left"),
                                                                   # actionButton("atualizar2","Atualizar", width = '145px'),
                                                                   div(actionButton("reiniciar","Reiniciar", width = '145px'), style="float:right"),
                                                                   # div(uiOutput("botao_reiniciar"), style="float:right"),
                                                                   # actionButton("reiniciar","Reiniciar", width = '145px'),
                                                                   # uiOutput("botao_reiniciar"),
                                                                   div(uiOutput("botao_reiniciar"), style =  "padding-left: 0px; margin-top:75px"),
                                                                   
                                                                   # Esse painel aparece quando o botão atualizar é clicado
                                                                   conditionalPanel("output.condicao == 1 && input.atualizar2",
                                                                                    # div(style="display:inline-block; float:left",
                                                                                    #     selectInput("formato_mp", div("Exportar Resultados:", style = "font-size:96%"), width = '145px',
                                                                                    #                 choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"), selected = ".csv")),
                                                                                    # div(style="display:inline-block; float:right; margin-top:25px", width = '700px',
                                                                                    #     downloadButton('download_mp', 'Download')))
                                                                                    fluidRow(
                                                                                      column(6, selectInput("formato_mp", div("Exportar Resultados:", style = "font-size:96%;"), 
                                                                                                            choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"), selected = ".csv"),
                                                                                             style = "width:175px; margin-top: 0px; float:left;"),
                                                                                      # column(3, downloadButton('plot1_dl', 'Left Plot')),
                                                                                      column(3, downloadButton('download_mp', 'Download'), style = "width:145px; margin-top: 25px; float:right; ")
                                                                                    ),
                                                                                    # tags$style(type='text/css', "#plot1_dl { width:100%; margin-top: 25px;}"),
                                                                                    tags$style(type='text/css', "#download_mp { width:145px; margin-top: 0px; float:right; }"),
                                                                                    tags$style(type='text/css', "#formato_mp { width:145px; margin-top: 0px; float:right; }"),
                                                                                    tags$style(type='text/css', ".selectize-dropdown-content {max-height: 75px; }")
                                                                   )
                                                                   # selectInput("formato_mp", div("Exportar Resultados:", style = "font-size:96%"),
                                                                   #             choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"), selected = ".csv"),
                                                                   # downloadButton('download_mp', 'Download'))
                                                          )
                                         )
                                   ),
                                   
                                   # Painel com os resultados do modelo
                                   box(status = "primary", width = 8, title = strong("Resultados", style = "font-size:90%"),
                                       
                                       # Esse painel aparece quando o arquivo foi carregado
                                       conditionalPanel("output.arquivo_carregado_mp2", 
                                                        
                                                        # Esse painel aparece quando o botão "atualizar" é acionado
                                                        conditionalPanel("output.condicao == 1 && input.atualizar2",
                                                                         # verbatimTextOutput("progress_bar"),
                                                                         tabBox( width = "100%",
                                                                                 # verbatimTextOutput("progress_bar"),
                                                                                 tabPanel("Análise Gráfica", 
                                                                                          # verbatimTextOutput("tempo_mp"),
                                                                                          dygraphOutput("mp_graficoseries")),
                                                                                 tabPanel("Resumo do ajuste", verbatimTextOutput("summary_mpseries")),
                                                                                 tabPanel("Análise dos Resíduos", dygraphOutput("mp_grafico_res_series")),
                                                                                 tabPanel("Exclusão de Variáveis", uiOutput("choose_columns_series")#,
                                                                                          # verbatimTextOutput("aux_correcao")
                                                                                          # verbatimTextOutput("checkbox_series2"),
                                                                                          # verbatimTextOutput("outro_teste"),
                                                                                          # verbatimTextOutput("teste_dadosmp3"),
                                                                                          # verbatimTextOutput("teste_eventReactive"),
                                                                                          # verbatimTextOutput("series_novomodelo"),
                                                                                          # verbatimTextOutput("nomes_novomodelo")
                                                                                 ),
                                                                                 conditionalPanel("input.novo_modelo",
                                                                                                  verbatimTextOutput("novomodelo"))
                                                                         ))
                                                        
                                       )
                                   )
                                   ) # Fim bsCollapsePanel
                   ) # Fim bsCollapse
),
                  
                  # Item: Previsão -------------------------------------------------------------------------------------------
                  tabItem(tabName = "previsao",
                          
                          bsCollapse(id = "collapse_previsao", open = 1,
                                     bsCollapsePanel(value = 1, style = "default",
                                                     title = span(icon("umbrella"),"Previsão", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                                                     
                                                     div("Obtenha previsões 12 passos à frente para a sua série temporal de interesse.
                                                         O método utilizado é a Abordagem Box & Jenkins para séries temporais, em que é ajustado um modelo da classe ARIMA."),
                                                     p("Para saber mais sobre a metodologia empregada, acessa a opção", tags$i("Saiba mais"),"."),
                                                     
                                                     
                                                     hr(),
                                                     
                                                     # Importar arquivo .xlsx local
                                                     tipify(fileInput('arquivo_local_arima', label = p("Importar arquivo .csv:", style = "font-size:96%"),multiple = FALSE, accept = '.csv'),
                                                            "Apenas arquivos em formato .csv",
                                                            placement = "top", trigger = "hover"),
                                                     
                                                     conditionalPanel("!output.arquivo_carregado_arima",
                                                                      p(tags$b("OBS1:"),"Funciona apenas para séries temporais mensais.", style = "font-size:90%; color:#2C65A1"),
                                                                      p(tags$b("OBS2:"),"Para importar a sua série temporal, crie um arquivo", tags$b(".csv"), "com as seguintes colunas.",
                                                                        style = "font-size:90%; color:#2C65A1"),
                                                                      img(src="importar.png")
                                                     ),
                                                     
                                                     hr(),
                                                     
                                                     conditionalPanel("output.arquivo_carregado_arima",
                                                                      
                                                                      tabBox(width = "100%",
                                                                             tabPanel("Especificações do ajuste",
                                                                                      strong("Modelo Ajustado:", style = "font-size:96%"),
                                                                                      verbatimTextOutput("texto_arima"),
                                                                                      strong("Teste de significância dos parâmetros:",style = "font-size:96%"),
                                                                                      verbatimTextOutput("t_test_arima")),
                                                                             tabPanel("Gráfico", dygraphOutput("arima_grafico")),
                                                                             tabPanel("Previsão",
                                                                                      dygraphOutput("arima_previsao"),
                                                                                      hr(),
                                                                                      tableOutput("tabela_previsao")),
                                                                             tabPanel("Resíduos", dygraphOutput("arima_grafico_res")),
                                                                             tabPanel("Exportar Resultados",
                                                                                      p("Os resultados disponíveis para download são os valores ajustados, as previsões para
                                                                                        os próximos 12 meses e os resíduos do modelo da classe ARIMA."),
                                                                                      selectInput("formato_arima", div("Exportar Resultados:", style = "font-size:96%"),
                                                                                                  choices = c(".csv", ".xlsx",".txt",".dta", ".sas", ".spss"),
                                                                                                  selected = ".csv" ),
                                                                                      downloadButton('download_arima', 'Download'))
                                                                             ))
                                                     
                                                     
                                                     
                                     ))
                          
                  ),

                #Item: Analise Macro --------------------------
                
#                 tabItem(tabName = 'atividade', 
#                         #                         p("Faremos um exercício de estresse: para vários cenários de depreciação cambial será calculado o valor do aumento da dívida das empresas como proporção do seu EBITDA acumulado nos últimos 12 meses.5 No exercício, as empresas cujo aumento de dívida superar dois anos de EBITDA serão consideradas falidas e seu EBITDA deixará de existir. Como mostra o Gráfico 1, no cenário do IBRE (de câmbio a R$ 4,70 por dólar ao final de 2016), menos de 5,0% das empresas iriam à falência por esse critério."),
#                         #                         br(),
#                         #                         htmlOutput("falencia"),
#                         #                         br(), 
#                         #                         br(),
#                         #                         p("Todavia, como indica o Gráfico 2, isso corresponde a 6,5% do EBITDA deste grupo.Uma parcela de 6,5% de 13% da economia representa uma contração da atividade econômica de 0,8%. Como apontado no Gráfico 2, o cenário fica muito mais desafiador quando o câmbio atinge R$ 8,00/US$, patamar que seria capaz de deprimir a atividade econômica em até 4,0%."),
#                         #                         br(),
#                         #                         p("Essa análise fornece uma visão, ainda que preliminar, do argumento de que o efeito da desvalorização cambial no balanço das empresas é maior do que o efeito sobre a rentabilidade das exportações. Evidentemente, para isso ainda seria necessário considerar os efeitos da desvalorização cambial sobre as quantidades exportadas e importadas. "),
#                         #                         br(),
#                         #                         htmlOutput("falencia2"),
#                         #                         br(),
#                         #                         p("o cenário cambial esperado para 2015 e 2016 deve contribuir para o aprofundamento da recessão observada neste ano. Em linha com a deterioração do cenário, como é possível inferir a partir da Tabela 1, o Monitor do PIB de agosto de 2015 registrou, na comparação com agosto de 2014, contração de 4,2%. Os destaques negativos são a importação (com contração interanual de 23,3%) e a formação bruta de capital fixo (com contração interanual de 16,9%). Pela ótica da oferta, permanece a deterioração do setor de serviços, com intensificação da contração da atividade industrial, causada pela piora marginal do setor de construção civil."),
#                         #                         br(),
#                         
#                         bsCollapse(id = "collapse_atividade", open = 1, 
#                                    bsCollapsePanel(value = 1, style = "default",
#                                                    title = span("Monitor do PIB", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
#                                                    dataTableOutput("monitor_pib")))
#                         
#                         
#                         #                         bsCollapse(id = "collapse_atividade", open = 1,
#                         #                                    
#                         #                                    bsCollapsePanel(value = 1, style = "default",p("Atividade Econômica "),
#                         #                                                    br(),
#                         #                                                    p("Faremos um exercício de estresse: para vários cenários de depreciação cambial será calculado o valor do aumento da dívida das empresas como proporção do seu EBITDA acumulado nos últimos 12 meses.5 No exercício, as empresas cujo aumento de dívida superar dois anos de EBITDA serão consideradas falidas e seu EBITDA deixará de existir. Como mostra o Gráfico 1, no cenário do IBRE (de câmbio a R$ 4,70 por dólar ao final de 2016), menos de 5,0% das empresas iriam à falência por esse critério."),
#                         #                                                    br(),
#                         #                                                    htmlOutput("falencia"),
#                         #                                                    br(),
#                         #                                                    br(),
#                         #                                                    p("Todavia, como indica o Gráfico 2, isso corresponde a 6,5% do EBITDA deste grupo.Uma parcela de 6,5% de 13% da economia representa uma contração da atividade econômica de 0,8%. Como apontado no Gráfico 2, o cenário fica muito mais desafiador quando o câmbio atinge R$ 8,00/US$, patamar que seria capaz de deprimir a atividade econômica em até 4,0%."),
#                         #                                                    br(),
#                         #                                                    p("Essa análise fornece uma visão, ainda que preliminar, do argumento de que o efeito da desvalorização cambial no balanço das empresas é maior do que o efeito sobre a rentabilidade das exportações. Evidentemente, para isso ainda seria necessário considerar os efeitos da desvalorização cambial sobre as quantidades exportadas e importadas. "),
#                         #                                                    br(),
#                         #                                                    htmlOutput("falencia2")))
#                         
#                 ),
#                 
#                 #> menu sondagens IBRE ---------------------
#                 tabItem(tabName = 'sondagens',
#                         
#                         bsCollapse(id = "collapse_atividade", open = 1, 
#                                    bsCollapsePanel(value = 1, style = "default",
#                                                    title = span("Sondagens", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
#                                                    box(width = 10,  solidHeader = T, dygraphOutput("sondagens")))),
#                         bsCollapse(id = "collapse_atividade", open = 1, 
#                                    bsCollapsePanel(value = 1, style = "default",
#                                                    title = span("Sondagen de Serviço", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
#                                                    box(width = 10,  solidHeader = T, dygraphOutput("sondagens_servico")))),
#                         
#                         bsCollapse(id = "collapse_atividade", open = 1, 
#                                    bsCollapsePanel(value = 1, style = "default",
#                                                    title = span("Sondagen da Indústria", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
#                                                    box(width = 10,  solidHeader = T, dygraphOutput("sondagens_industria")))),
#                         
#                         bsCollapse(id = "collapse_atividade", open = 1, 
#                                    bsCollapsePanel(value = 1, style = "default",
#                                                    title = span("Sondagen do Comércio", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
#                                                    box(width = 10,  solidHeader = T, dygraphOutput("sondagens_comercio")))),
#                         bsCollapse(id = "collapse_atividade", open = 1, 
#                                    bsCollapsePanel(value = 1, style = "default",
#                                                    title = span("Sondagen da Construção", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
#                                                    box(width = 10,  solidHeader = T, dygraphOutput("sondagens_construcao")))),
#                         bsCollapse(id = "collapse_atividade", open = 1, 
#                                    bsCollapsePanel(value = 1, style = "default",
#                                                    title = span("Sondagen da Consumidor", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
#                                                    box(width = 10,  solidHeader = T, dygraphOutput("sondagens_consumidor"))))
#                         
#                         
#                 ),
                  
                  # Item: Sobre  ------------------------------------------------------------------------------------------- #
                  tabItem(tabName = "sobre",
                          bsCollapse(id = "collapse_sobre", open = 1,
                                     bsCollapsePanel(value = 1, style = "default",
                                                     title = span(icon("question"),"Sobre", style = "font-size:90%;font-weight:bold;color:#2C65A1"),
                          
                          p("O", tags$b(tags$em("Smart"), "IBRE"), "foi elaborado com o intuito de familiarizar o usuário com as séries
                              temporais disponibilizadas pela Fundação Getúlio Vargas, mostrando sua utilidade e importância na conjuntura
                                                       econômica, e de oferecer ferramentas de análise de séries temporais.",style = "text-align:justify"),
                          p("Nas abas abaixo você pode saber um pouco mais sobre cada seção do programa
                            e sobre as metodologias aplicadas nas ferramentas de ajuste sazonal, previsão e modelo paramétrico.",style = "text-align:justify"),
                          
                                                     
                          tabBox(width = 12,
                            tabPanel(tags$b("Seções"), 
                          
                          p("Breve descrição sobre as seções:"),
                          
                          p(icon("table"),strong("Banco de ST's:"),  
                            "Nessa seção você encontra as séries disponíveis para análise e download no Smart IBRE.
                            Você pode visualizar gráficos e tabelas com os dados e informações resumidas.",
                            align = "justify"),
                          
                          p(icon("umbrella"),strong("Previsão:"),
                            "Insira sua série mensal e obtenha a previsão 12 passos à frente através do ajuste automático de um modelo
                            da classe ARIMA. Você também pode verificar o gráfico, a especificação do modelo ajustado e analisar o
                            comportamento dos resíduos. Os resultados que pode ser exportados são os dados origniais, 
                            as previsões e os resíduos do modelo.", align = "justify"),
                          
                          p(icon("line-chart"),strong("Ajuste Sazonal:"), 
                            "Aqui você pode dessazonalizar uma série temporal. Tal ato é feito pelo X13-ARIMA-SEATS (U.S. Census Bureau).
                            Para avaliar a qualidade do ajuste, observe a aba 'Diagnósticos'. É permitido personalizar o ajuste, porém, inicialmente é
                            feito um ajuste automático. Está disponível uma previsão para os doze passos (meses) à frente.
                            Também é possível fazer o download dos resultados que estão na aba 'Dados'.
                            Para importar sua série temporal, certifique-se que o formato do arquivo é .csv com 2 colunas contendo
                            a data e os valores da série temporal. ", align = "justify"),
                          
                          p(icon("exchange"),strong("Modelo Paramétrico:"), 
                            "Na aba 'Automático' você é capaz de ajustar uma regressão linear usando as séries da FGV como covariáveis.
                            Em 'Pesos', você pode fixar os coeficientes para cada covariável, criando seu próprio índice paramétrico.
                            Em 'Resultados', acesse o gráfico do índice ajustado, as estatísticas-resumo da regressão linear e salve
                            os valores ajustados e os resíduos do modelo final.", align = "justify")
                          ),
                          tabPanel(tags$b("Links úteis"),
                                   div("Todo esse programa foi desenvolvido utilizando o software R e o RStudio."),
                                   div("Nós disponibilizamos alguns links que podem te ajudar a aprimorar seu desenvolvimento nesses programas:"),
                                   br(),
                                   br(),
                                   fluidRow(
                                     box( title = p("Básico", style = "font-size:90%; font-weight:bold"), width = 4, status = NULL,
                                          
                                          a("01 Instalação dos softwares R e RStudio", style = "color:steelblue;font-size:96%", href = "https://www.youtube.com/watch?v=8HQHf5XCS7g&index=1&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                          a("02 Operações Básicas", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=thekHZaX60w&index=2&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                          a("03 Vetores, Matrizes e Data frames", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=sjQAdscdF4E&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=3", target="_blank"), br(),
                                          a("04 Script",style = "color:steelblue;font-size:96%", href = "https://www.youtube.com/watch?v=cjCA28Q4LeU&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=4", target="_blank"), br(),
                                          a("05 Estruturas de Condição", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=Qz199rCFFzg&index=5&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                          a("06 Estruturas de Repetição", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=wfUVTI8OsSQ&index=6&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                          a("07 Como Criar Funções", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=pEQKwxzOBiA&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=7", target="_blank"), br(),
                                          a("08 Pacotes", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=nBafys2Y_P4&index=8&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                          a("09 Diretório de Trabalho", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=uIviGsyuqMA&index=9&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                          a("10 Ler e Salvar dados", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=1Y9htomXoGU&index=10&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank")
                                          
                                     ),
                                     box(title = p("Estatística Descritiva", style = "font-size:90%; font-weight:bold"),  width = 4, status = NULL,
                                         a("11 Média", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=bf0_5VmfrC0&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=11", target="_blank"), br(),
                                         a("12 Mediana", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=TWlj6h1zZ2w&index=12&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                         a("13 Moda", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=tBuNkzrORLE&index=13&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                         a("14 Variância e Desvio-Padrão", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=lCVX6ZiNPV4&index=14&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                         a("15 Coeficiente de Variação", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=q6shyJt-FE0&index=15&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                         a("16 Quantis e Percentis", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=pZN-RmV4M6E&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=16", target="_blank"), br(),
                                         a("17 Coeficiente de Assimetria", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=Wxv4J3fDhr8&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=17", target="_blank"), br(),
                                         a("18 Histograma",style = "color:steelblue;font-size:96%", href = "https://www.youtube.com/watch?v=c0Y4sYUOsBQ&index=18&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS", target="_blank"), br(),
                                         a("19 Boxplot", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=f8kjIDPh0Ow&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=19", target="_blank"), br(),
                                         a("20 Gráficos de Pizza e de Barras", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=vVktvPu3uIg&list=PLuQWWXrHQLiejECp_ldRPEjuVnJrG7fVS&index=20", target="_blank")
                                         
                                     ),
                                     
                                     box(title = p("Ajuste Sazonal", style = "font-size:90%; font-weight:bold"),  width = 4, status = NULL,
                                         a("01 Primeiros Passos", style = "color:steelblue;font-size:96%",href = "https://www.youtube.com/watch?v=vFarvV_dy9U&list=PLuQWWXrHQLidnGIkFwe1fwkxjTQ-NWZ0W&index=1", target="_blank"), br(),
                                         a("02 Dessazonalização da série temporal Airline",style = "color:steelblue;font-size:96%", href = "https://www.youtube.com/watch?v=Tnjsik2ClT4&index=2&list=PLuQWWXrHQLidnGIkFwe1fwkxjTQ-NWZ0W", target="_blank")
                                         
                                     ))),
                          
                          tabPanel(tags$b("Metodologias"),
                                   div("Para desenvolver o Smart IBRE, foram utilizadas diversas metodologias."),
                                   div("Nós disponibilizamos alguns links que podem te ajudar a entendê-las:"),
                                   br(),
                                   br(),
                                   
                                   fluidRow(
                                     box(title = p("Ajuste Sazonal", style = "font-size:90%; font-weight:bold"),  width = 4, status = NULL,  
                                         a("01 X-13ARIMA-SEATS Reference Manual", style = "color:steelblue;font-size:96%", href = "https://www.census.gov/ts/x13as/docX13AS.pdf", target="_blank"),br(),
                                         a("02 Métodos de ajuste sazonal para séries de Business Tendency", style = "color:steelblue;font-size:96%", href = "http://bit.ly/NT-X13_PDF", target="_blank"),br(),
                                         a("03 X-13ARIMA-SEATS com R: Um Estudo de Caso para a Produção Industrial Brasileira", style = "color:steelblue;font-size:96%", href = "http://bit.ly/TD-80", target="_blank")
                                         
                                     ),
                                     box(title = p("Modelo Paramétrico", style = "font-size:90%; font-weight:bold"),  width = 4, status = NULL,  
                                         "Adicionar metodologia."
                                         
                                     ),
                                     box(title = p("Previsão", style = "font-size:90%; font-weight:bold"),  width = 4, status = NULL,  
                                         "Adicionar metodologia."
                                         
                                     )
                                     
                                   )
                                   
                          )
                          )
                          
                         
          
                          
                          
                          )
                          ))
                  
                  
                  
                          ) # fim tabItems
                
                
                
                
                
                          ) # fim BODY
              
                  ) # fim PAGE - fim total
