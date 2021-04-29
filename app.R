
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(thematic)
library(crul)
library(readr)
shinyWidgetsGallery()

if(!exists("brasil_df")) {
  source('auxiliar.R',encoding = 'UTF-8')
}

customLogo <- shinyDashboardLogoDIY(
    boldText = "Auxílio"
    ,mainText = "Emergencial"
    ,textSize = 16
    ,badgeText = "v1"
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
                       ,icon("linkedin"), "Meu Perfil", target="_blank")),
        tags$li(class="dropdown",
                tags$a(href="#",
                       icon("github"), "Código Fonte", target="_blank"))
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = 'Apresentação',
                tabName = 'apresentacao'
            ),
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
            )
        )
    ),
    dashboardBody(
        shinyDashboardThemes('blue_gradient'),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            ##### Apresentação ----
            tabItem(
                tabName = 'apresentacao',
                h2('aaaaaaaaaaaaaaaaaaa'),
            ),
            #####  Brasil - Informações ----
            tabItem(
                tabName = 'informacao_brasil',
                fluidRow(
                    valueBoxOutput('total_pago_brasil',width = 4),
                    valueBoxOutput('total_beneficiarios_brasil',width = 4),
                    valueBoxOutput('media_pagamento_anual',width = 4),
                    valueBoxOutput('porcentagem_beneficiada',width = 4),
                    valueBoxOutput('media_valor_pago',width = 4),
                    box(
                        title = 'Distribuição do auxílio emergencial',
                        status = 'primary',
                        color="red",
                        solidHeader = TRUE,
                        width = 8,
                        collapsible = TRUE,
                        leafletOutput('mapa_br_aux',height = 400)
                    ),
                    box(
                        title = 'Distribuição do auxílio emergencial',
                        status = 'primary',
                        color="red",
                        solidHeader = FALSE,
                        width = 4,
                        collapsible = TRUE,
                        awesomeRadio(
                          inputId = 'radiomapabrasil',
                          label = 'Mapa de Calor:',
                          choices = c('Quantidade de Beneficiários' = 'QUANTIDADE.DE.BENEFICIÁRIOS',
                                      'Total Disponibilizado' = 'TOTAL.DISPONIBILIZADO'),
                          selected = 'QUANTIDADE.DE.BENEFICIÁRIOS',
                          status = 'success'
                        ),
                        awesomeCheckbox(
                          inputId = 'exibir_legendas_br',
                          label = 'mostrar legenda?'
                        ),
                    ),
                    box(
                        title = 'Top 5 Estados Por valor médio anual',
                        status = 'primary',
                        solidHeader = TRUE,
                        width = 12,
                        collapsible = TRUE,
                        plotlyOutput('graf_top5')
                    ),
                )
            ),
            #####  Brasil - Base de dados ----
            tabItem(
                tabName = 'base_brasil',
                h3('ccccccccccccccccccc')
            ),
            #####  Bahia - Informações ----
            tabItem(
                tabName = 'informacao_bahia',
                h3('Bahia - Informações')
            ),
            #####  Bahia - Base de dados ----
            tabItem(
                tabName = 'base_bahia',
                h3('Bahia - Base de dados')
            )
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    #### Função Paleta de Cores - Mapa do brasil
    pal <- reactive({
      if (input$radiomapabrasil == 'QUANTIDADE.DE.BENEFICIÁRIOS'){
        pal <- colorNumeric(palette = "Reds",brasil_df[[input$radiomapabrasil]])
      }
      else {
        pal <- colorNumeric(palette = "Greens",brasil_df[[input$radiomapabrasil]])
      }
    })
    
    palcolor <- reactive({
      pal <- pal()
      ~pal(brasil_df[[input$radiomapabrasil]]) 
    })
    #### Adicionar e retirar legenda - mapa do brasil
    observe({
      proxy <- leafletProxy("mapa_br_aux", data = brasil_df)
      
      proxy %>% clearControls()
      if (input$exibir_legendas_br){
        pal <- pal()
        proxy %>%
          addLegend("bottomright",
                    title = "aaaaaaaa", 
                    pal = pal, 
                    values = ~brasil_df[[input$radiomapabrasil]], 
                    opacity = 0.8)
      }
    })
    
    
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
                                    "<b>Estado: </b>", name_state, "<br>",
                                    "<b>Quantidade beneficiados: </b>", brasil_df[[input$radiomapabrasil]], "<br>"))
    })
    ##### Gráfico Top 5
    output$graf_top5 <- renderPlotly({
        g1 <-cars %>%
            ggplot(aes(x=speed,y=dist)) +
            geom_point()
        g1 %>% ggplotly()
    })
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
        subtitle = 'Total de Beneficiários',
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
        subtitle = 'Porcentagem da População Beneficiada',
        color = 'teal',
        icon = icon('percentage')
      )
    })
    #### Média valor pago (mensal)
    media_valor_pago_mensal <- reactive({
      round(sum(brasil_df_raw$`TOTAL DISPONIBILIZADO`)/sum(brasil_df_raw$`QUANTIDADE DE BENEFICIÁRIOS`),0)/9
      
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

    