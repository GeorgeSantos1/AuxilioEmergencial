
library(shiny)
library(ggplot2)
library(qs)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(readr)
library(forcats)
library(reactable)
library(DT)
library(sf)
library(scales)
library(shinycssloaders)
library(dplyr)
library(ggthemes)
library(readr)
library(sp)
library(tippy)

options(encoding = "UTF-8")

source("modulos/rtexto.R",encoding = "UTF-8")
source("auxiliar.R",encoding = "UTF-8")

####
customLogo <- shinyDashboardLogoDIY(
    boldText = "Auxílio"
    ,mainText = "Emergencial"
    ,textSize = 16
    ,badgeText = "V.1"
    ,badgeTextColor = "white"
    ,badgeTextSize = 2
    ,badgeBackColor = "#40E0D0"
    ,badgeBorderRadius = 3
)

#### Inicio da Interface -----------
ui <- dashboardPage(
  title = 'Auxílio_Emergencial',
    dashboardHeader(
        title = customLogo,
        tags$li(class="dropdown",
                tags$a(href="#"
                       ,icon("linkedin"), "Meu Perfil", target="_blank"))
    ),
  
    dashboardSidebar(
        sidebarMenu(
          id = 'sidebar',
            menuItem(
                text = 'Brasil',
                tabName = 'brasil',
                menuSubItem(text = 'Informações',
                            tabName = 'informacao_brasil'),
                menuSubItem(text = 'Base de dados',
                            tabName = 'base_brasil')
            ),
            menuItem(
                text = 'Bahia',
                tabName = 'bahia',
                menuSubItem(text = 'Informações',
                            tabName = 'informacao_bahia'),
                menuSubItem(text = 'Base de dados',
                            tabName = 'base_bahia')
            ),
            menuItem(
                text = 'Sobre',
                tabName = "sobre"
          )
        )
    ),
    dashboardBody(
        shinyDashboardThemes('blue_gradient'),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            #####  Brasil - Informações ----
            tabItem(
                tabName = 'informacao_brasil',
                fluidRow(
                    valueBoxOutput('total_pago_brasil',width = 4),
                    valueBoxOutput('total_beneficiarios_brasil',width = 4),
                    valueBoxOutput('media_pagamento_anual',width = 4),
                    valueBoxOutput('porcentagem_beneficiada',width = 4),
                    valueBoxOutput('media_valor_pago',width = 4),
                    column(
                      width = 12,
                      box(
                        title = 'Distribuição do Auxílio Emergencial',
                        status = 'primary',
                        color="red",
                        solidHeader = TRUE,
                        width = 8,
                        collapsible = TRUE,
                        leafletOutput('mapa_br_aux',height = 400) %>% 
                          withSpinner(type=4,
                                      color = '#40e0d0')
                      ),
                      box(
                        status = 'primary',
                        color="red",
                        solidHeader = FALSE,
                        width = 4,
                        collapsible = TRUE,
                        awesomeRadio(
                          inputId = 'radiomapabrasil',
                          label = 'Tematizar com:',
                          choices = c('Quantidade de Beneficiados' = 'QUANTIDADE_DE_BENEFICIARIOS',
                                      'Total Disponibilizado' = 'TOTAL.DISPONIBILIZADO',
                                      '% da População Beneficiada' = 'porcentagem_beneficiada'),
                          selected = 'QUANTIDADE_DE_BENEFICIARIOS',
                          status = 'success'
                        ),
                        awesomeCheckbox(
                          inputId = 'exibir_legendas_br',
                          label = 'Mostrar Legenda?'
                        ),
                      ),
                    ),
                    column(
                      width = 12,
                      box(
                        status = 'primary',
                        solidHeader = FALSE,
                        width = 4,
                        collapsible = TRUE,
                        awesomeRadio(
                          inputId = 'top_or_bottom_choice',
                          label = 'Selecione a Variável: ',
                          choices = c('Quantidade de Beneficiados' = "QUANTIDADE DE BENEFICIÁRIOS",
                                      'Total Disponibilizado' = "TOTAL DISPONIBILIZADO",
                                      '% da População Beneficiada' = 'porcentagem_beneficiada'),
                          selected = "QUANTIDADE DE BENEFICIÁRIOS",
                          status = 'success'
                        ),
                        tippy::with_tippy(
                          div(
                            style = "text-align: center;",
                            switchInput(
                              inputId = "top_or_bottom",
                              label = 'Estados com:',
                              value = TRUE,
                              onLabel = 'Maior Quantidade',
                              size = 'normal',
                              offLabel = 'Menor Quantidade',
                              labelWidth = "100px",
                              handleWidth = "100px",
                              offStatus = 'danger'
                            ),
                          ),
                          tooltip = 'Clique Aqui Para Modificar o Gráfico'
                        )
                      ),
                      box(
                        title = 'Principais Estados Beneficiados',
                        status = 'primary',
                        solidHeader = TRUE,
                        width = 8,
                        collapsible = TRUE,
                        plotlyOutput('graf_top5') %>% 
                          withSpinner(type=4,
                                      color = '#40e0d0')
                      ),
                    ),
                )
            ),
            #####  Brasil - Base de dados ----
            tabItem(
                tabName = 'base_brasil',
                box(
                  width = 12,
                  status = 'primary',
                  p('Base de dados com informações sobre a distribuição do auxílio 
                    emergencial nos estados brasileiros no ano de 2020.',class='aba_base_brasil'),
                  DT::DTOutput('databrasil'),
                  downloadButton('down','Baixar (.CSV)',class = 'download_button')
                )
            ),
            #####  Bahia - Informações ----
            tabItem(
                tabName = 'informacao_bahia',
                nameui("nameui")
            ),
            #####  Bahia - Base de dados ----
            tabItem(
                tabName = 'base_bahia',
                box(
                  width = 12,
                  status = 'primary',
                  p('Base de dados com informações sobre a distribuição do auxílio 
                    emergencial nos municípios baianos no ano de 2020.',class="aba_base_brasil"),
                  DT::DTOutput('databahia'),
                  downloadButton('down1','Baixar (.CSV)',class = 'download_button')
                )
            ),
            tabItem(
              tabName = 'sobre',
              div(class = 'sobre',
                  h3('Data de Atualização'),
                  p('Última atualização do dashboard foi realizada em 21/06/2021.'),
                  h3('Contexto'),
                  p('Como tentativa de instaurar medidas de proteção social e atenuar a crise econômica decorrente aos 
                    efeitos causados pela pandemia de COVID-19, no Brasil foi criado o auxílio emergencial.'),
                  p("O auxílio emergencial (também chamado de Caixa Auxílio Emergencial ou coronavoucher) é um benefício 
                    instituído no Brasil pela Lei de nº 13.982/2020, que previu o repasse de 600 reais mensais 
                    (inicialmente por três meses) a trabalhadores informais e de baixa renda, microempreendedores individuais
                    e também contribuintes individuais do Instituto Nacional do Seguro Social (INSS). No ano de 2020,
                    o auxílio começou a ser pago no mês de abril, no valor de R$ 600,00 (seiscentos reais), e foi até o mês 
                    de dezembro. "),
                  h3('Fonte dos dados'),
                  tags$ul(
                    tags$li(
                      p('API do ',a('Portal da transparência:',href='http://www.portaltransparencia.gov.br/'),
                      tags$u(tags$em(' Total pago')),' e',tags$u(tags$em(' Quantidade de beneficiados')),' para estados e municípios.'),
                    ),
                    tags$li(
                      p(a("IBGE:",href='ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2020/estimativa_dou_2020.pdf'),
                        'Estimativas para', tags$u(tags$em(' População')), 'dos estados e municípios')
                    )
                  ),
                  h3("Responsável"),
                  p('George Anderson Alves dos Santos'),
                  p('Estatístico - Analista de dados - Cientista de dados'),
                  p('Email:',a('george_anderson',href='mailto:george_13031995@hotmail.com'))
                  
              )
            )
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  callModule(name, "nameui")

# Mapa Do Brasil ----------------------------------------------------------

    #### Função Paleta de Cores - Mapa do brasil
    pal <- reactive({
      
      if (input$radiomapabrasil == 'QUANTIDADE_DE_BENEFICIARIOS'){
        pal <- colorNumeric(palette = "Reds",brasil_df[[input$radiomapabrasil]])
      }
      else if (input$radiomapabrasil == 'TOTAL.DISPONIBILIZADO'){
        pal <- colorNumeric(palette = "Greens",brasil_df[[input$radiomapabrasil]])
      }
      else {
        pal <- colorNumeric(palette = "PuBu",brasil_df[[input$radiomapabrasil]])
      }
    }) %>%
      bindCache(input$radiomapabrasil)
    
    palcolor <- reactive({
      pal <- pal()
      ~pal(brasil_df[[input$radiomapabrasil]]) 
    }) %>%
      bindCache(input$radiomapabrasil, pal())
    #### Adicionar e retirar legenda - mapa do brasil
    observe({
      proxy <- leafletProxy("mapa_br_aux", data = brasil_df)
    
      if (input$radiomapabrasil == 'QUANTIDADE_DE_BENEFICIARIOS'){
        legenda_t <- 'Quantidade de Beneficiados'
      } else if (input$radiomapabrasil == 'TOTAL.DISPONIBILIZADO') {
        legenda_t <- 'Total Disponibilizado'
      } else {
        legenda_t <- '% Beneficiados'
      }
      proxy %>% clearControls()
      if (input$exibir_legendas_br){
        pal <- pal()
        proxy %>%
          addLegend("bottomright",
                    title = legenda_t,
                    pal = pal, 
                    values = ~brasil_df[[input$radiomapabrasil]], 
                    opacity = 0.8)
      }
    }) %>% bindEvent(input$radiomapabrasil,pal(),input$exibir_legendas_br)
    
    ##### Mapa Brasil
    output$mapa_br_aux <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data=brasil_df,
                    smoothFactor = 0.5,
                    fillOpacity = 0.4,
                    weight = 0.5,
                    opacity = 0.8,
                    label = ~name_state,
                    color = palcolor(),
                    highlightOptions = highlightOptions(color = "black",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    popup = ~paste0(sep = " ",
                                    strong('Estado: '), name_state, "<br>",
                                    strong('Quantidade de Beneficiados: '), 
                                    formatC(
                                        `QUANTIDADE_DE_BENEFICIARIOS`,
                                        big.mark='.', format = 'fg',
                                        decimal.mark = ','
                                    ),"<br>",
                                    strong('Total Disponibilizado: '),
                                    formatC(
                                      TOTAL.DISPONIBILIZADO,
                                      big.mark='.', format = 'fg',
                                      decimal.mark = ','
                                    ), "<br>",
                                    strong('% de Beneficiados: '),
                                    porcentagem_beneficiada, ' %' , "<br>"
                              )
                    ) %>%
        setView(lat = -14.2401,lng = -53.1805,zoom = 4)
    }) %>% bindCache(palcolor())

    
# Gráfico Top/Bottom 10 - Brasil ------------------------------------------

    ##### Dados Filtrados - Gráfico Top/Bot 10
    data_br_filtrado <- reactive({
      
      if (input$top_or_bottom == TRUE){
        
        df <- brasil_df_raw %>% 
          select(name_state,.data[[input$top_or_bottom_choice]]) %>%
          arrange(desc(.data[[input$top_or_bottom_choice]])) %>%
          mutate(name_state = fct_reorder(name_state,
                                          .data[[input$top_or_bottom_choice]])) %>%
          head(10)
      }
      else {
        
        df <- brasil_df_raw %>% 
          select(name_state,.data[[input$top_or_bottom_choice]]) %>%
          arrange(.data[[input$top_or_bottom_choice]]) %>%
          mutate(name_state = fct_reorder(name_state,
                                          .data[[input$top_or_bottom_choice]]),
                 name_state = fct_rev(name_state)) %>%
          head(10)
      }
    }) %>% bindCache(input$top_or_bottom_choice,input$top_or_bottom)
    
    #### Customização de Fonte/Rótulo para gráfico Top/Bot 10 Brasil
    font <- list(
      family = "titillium web",
      size = 15,
      color = "white"
    )
    label <- list(
      bgcolor = "#2f89a4",
      bordercolor = "transparent",
      font = font
    )
    
    #### Texto indicando se é top 10 ou bottom 10
    top_or_bot_10 <- reactive({
      if (input$top_or_bottom == TRUE){
        'Estados com maior '
      } 
      else {
        'Estados com menor '
      }
    })
    
    ##### Texto indicado qual variável está sendo utilizada
    texto_variavel <- reactive({
      if (input$top_or_bottom_choice == "QUANTIDADE DE BENEFICIÁRIOS"){
        'quantidade de beneficiados'
      } 
      else if (input$top_or_bottom_choice == "TOTAL DISPONIBILIZADO"){
        'total disponibilizado (R$)'
      }
      else {
        '% da população beneficiada'
      }
    })
    
    ##### Definição acurácia
    acuracia <- reactive({
      if (input$top_or_bottom_choice == 'porcentagem_beneficiada'){
        0.1
      }
      else {
        NULL
      }
    })
    
    ##### Gráfico Top/Bot 10
    output$graf_top5 <- renderPlotly({
      acuracia <- acuracia()
      
      brasil_df_raw_filtrado <- data_br_filtrado()
      
      titlep1 <- top_or_bot_10()
      titlep2 <- texto_variavel()
      
      g2 <- ggplot(brasil_df_raw_filtrado) +
        aes(
          x = name_state,
          weight = .data[[input$top_or_bottom_choice]],
          text=paste0(titlep2,': ',comma(.data[[input$top_or_bottom_choice]],
                                         big.mark = '.',
                                         decimal.mark = ',',
                                         accuracy = acuracia))) +
        geom_bar(aes(fill = .data[[input$top_or_bottom_choice]],
                     colour = .data[[input$top_or_bottom_choice]]),
                 width = 0.7) +
        coord_flip() +
        labs(
          title = paste(titlep1,titlep2, sep = ''),
          x = NULL,
          y = NULL
        ) +
        scale_fill_gradient(low='#1bece3',high ='#2f89a4') +
        scale_color_gradient(low = '#25c0b9',high = '#2b7187') +
        scale_y_continuous(labels = label_comma(
          big.mark = '.',
          decimal.mark = ','
        )) +
        # guides(fill=FALSE,colour = FALSE) +
        ggthemes::theme_hc() +
        theme(text = element_text(family = 'titillium web'),
              plot.title = element_text(family = 'titillium web',
                                   face = "bold",
                                   color = '#4d4d4d'),
              legend.position="none")
      ggplotly(g2,tooltip = 'text') %>%
        style(hoverlabel = label) %>%
        layout(font = font)
    })


# ValueBox - Brasil -------------------------------------------------------
    
    #### Total pago - Brasil
    total_pago_brasil <- reactive({
      paste('R$',
            formatC(
              round(formatC(brasil_df_raw %>%
                              select(`TOTAL DISPONIBILIZADO`) %>%
                              sum()
                            , big.mark='.', format = 'fg',
                            decimal.mark = ',',
                            big.interval = 6L) %>%
                      as.numeric()
                    ,0),format = 'fg',big.mark = ','
            ),'Bilhões'
      )
    })
    output$total_pago_brasil <- renderValueBox({
      valueBox(
        value= h3(total_pago_brasil(),class='value-box-h3',style='font-size: 25px;'),
        subtitle = 'Total Pago',
        color='teal',
        icon = icon('coins')
      )
    })
    #### Total beneficiários - Brasil
    total_beneficiarios_brasil <- reactive({
      brasil_df_raw %>%
        select(`QUANTIDADE DE BENEFICIÁRIOS`) %>%
        sum() %>%
        formatC(big.mark='.', format = 'fg',decimal.mark = ',')
    })
    
    output$total_beneficiarios_brasil <- renderValueBox({
      valueBox(
        value= h3(total_beneficiarios_brasil(),class='value-box-h3',style='font-size: 25px;'),
        subtitle = 'Total de Beneficiados',
        color='teal',
        icon = icon('user-check'))
    })
    
    #### Total População Brasileira
    output$media_pagamento_anual <- renderValueBox({
      valueBox(
        value = h3('211.755.692',class='value-box-h3',style='font-size: 25px;'),
        subtitle = 'População Brasileira (IBGE - 2020)',
        color = 'teal',
        icon = icon('users')
      )
    })
    
    #### Média valor pago (ano)
    media_valor_pago <- reactive({
      round(sum(brasil_df_raw$`TOTAL DISPONIBILIZADO`)/sum(brasil_df_raw$`QUANTIDADE DE BENEFICIÁRIOS`),0) %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',') %>%
        paste('R$ ',. ,',00',sep='')
    })
    output$media_valor_pago <- renderValueBox({
      valueBox(
        value = h3(media_valor_pago(),class='value-box-h3',style='font-size: 25px;'),
        subtitle = 'Valor Médio Pago (Anual)',
        color = 'teal',
        icon = icon('credit-card')
      )
    })
    
    #### Porcentagem da População Beneficiada
    porcentagem_beneficiada <- reactive({
      ((sum(brasil_df_raw$`QUANTIDADE DE BENEFICIÁRIOS`)/211755692)*100) %>%
        round(2) %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',') %>%
        paste('%')
    })
    output$porcentagem_beneficiada <- renderValueBox({
      valueBox(
        value = h3(porcentagem_beneficiada(),class='value-box-h3',style='font-size: 25px;'),
        subtitle = 'Percentual de Beneficiados (Diretamente)',
        color = 'teal',
        icon = icon('percentage')
      )
    })

# Dataset - Brasil --------------------------------------------------------
    
    ##### Filtrando dataset
    # dataset_br <- brasil_df_raw %>%
    #   rename('Estado' = name_state,
    #          'Qtd de Beneficiários' = `QUANTIDADE DE BENEFICIÁRIOS`,
    #          'Total Disponibilizado' = `TOTAL DISPONIBILIZADO`,
    #          'População Total' = `População total`,
    #          '% de beneficiados' = `porcentagem_beneficiada`) %>%
    #   mutate(`Qtd de Beneficiários` = paste(comma(`Qtd de Beneficiários`,
    #                                               big.mark = '.',
    #                                               decimal.mark = ',')),
    #          `População Total` = paste(comma(`População Total`,
    #                                          big.mark = '.',
    #                                          decimal.mark = ',')),
    #          `Total Disponibilizado` = paste(sep = '','R$ ',comma(`Total Disponibilizado`,
    #                                                               big.mark = '.',
    #                                                               decimal.mark = ','),',00'))
    ##### Renderizar dataset
    output$databrasil <- DT::renderDT(server = FALSE,
                                      DT::datatable(brasil_df_raw,
                                                    options = list(
                                                      dom = 'Bfrtip',
                                                      initComplete = JS(
                                                        "function(settings, json) {",
                                                        "$(this.api().table().header()).css({'background-color': '#1c5876', 'color': '#fff'});",
                                                        "}")
                                                      )) 
                                      )
    output$down <- downloadHandler(
      
      filename = function(){
        paste('Base_Brasil', ".csv", sep = "")
      },
      content = function(file){
        write.csv2(brasil_df_raw, file, row.names = FALSE)
      }
    )
    

# Dataset - Bahia ---------------------------------------------------------
    output$databahia <- DT::renderDT(server = FALSE,
                                     DT::datatable(bahia_df_raw,
                                                   options = list(
                                                     dom = 'Bfrtip',
                                                     initComplete = JS(
                                                       "function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#1c5876', 'color': '#fff'});",
                                                       "}")
                                                   )) 
    )
    
    output$down1 <- downloadHandler(
      
      filename = function(){
        paste('Base_Bahia', ".csv", sep = "")
      },
      content = function(file){
        write.csv2(bahia_df_raw, file, row.names = FALSE)
      }
    )

}
# Run the application 
shinyApp(ui = ui, server = server)