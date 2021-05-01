
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(crul)
library(readr)
library(forcats)
library(reactable)
library(DT)
library(scales)

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
                    column(
                      width = 12,
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
                                      'Total Disponibilizado' = 'TOTAL.DISPONIBILIZADO',
                                      '% da População Beneficiada' = 'porcentagem_beneficiada'),
                          selected = 'QUANTIDADE.DE.BENEFICIÁRIOS',
                          status = 'success'
                        ),
                        awesomeCheckbox(
                          inputId = 'exibir_legendas_br',
                          label = 'Mostrar legenda?'
                        ),
                      ),
                      box(
                        title = 'Distribuição do auxílio emergencial',
                        status = 'primary',
                        color="red",
                        solidHeader = TRUE,
                        width = 8,
                        collapsible = TRUE,
                        leafletOutput('mapa_br_aux',height = 400)
                      ),
                    ),
                    column(
                      width = 12,
                      box(
                        title = 'Top 5 Estados Por valor médio anual',
                        status = 'primary',
                        solidHeader = TRUE,
                        width = 8,
                        collapsible = TRUE,
                        plotlyOutput('graf_top5')
                      ),
                      box(
                        title = 'Top 5 Estados Por valor médio anual',
                        status = 'primary',
                        solidHeader = FALSE,
                        width = 4,
                        collapsible = TRUE,
                        awesomeRadio(
                          inputId = 'top_or_bottom_choice',
                          label = 'Gráfico de Colunas: ',
                          choices = c('Quantidade de Beneficiários' = "QUANTIDADE DE BENEFICIÁRIOS",
                                      'Total Disponibilizado' = "TOTAL DISPONIBILIZADO",
                                      '% da População Beneficiada' = 'porcentagem_beneficiada'),
                          selected = "QUANTIDADE DE BENEFICIÁRIOS",
                          status = 'success'
                        ),
                        div(
                          style = "text-align: center;",
                          switchInput(
                            inputId = "top_or_bottom",
                            label = "My label",
                            value = TRUE,
                            onLabel = '10+',
                            size = 'normal',
                            offLabel = '10-',
                            labelWidth = "100px",
                            handleWidth = "100px",
                            offStatus = 'danger'
                          )
                        )
                      ),
                    ),
                )
            ),
            #####  Brasil - Base de dados ----
            tabItem(
                tabName = 'base_brasil',
                p('Base de dados utilizada para criação dessa aba de dashboard.',class='aba_base_brasil'),
                DT::DTOutput('databrasil'),
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

# Mapa Do Brasil ----------------------------------------------------------

    #### Função Paleta de Cores - Mapa do brasil
    pal <- reactive({
      
      if (input$radiomapabrasil == 'QUANTIDADE.DE.BENEFICIÁRIOS'){
        pal <- colorNumeric(palette = "Reds",brasil_df[[input$radiomapabrasil]])
      }
      else if (input$radiomapabrasil == 'TOTAL.DISPONIBILIZADO'){
        pal <- colorNumeric(palette = "Greens",brasil_df[[input$radiomapabrasil]])
      }
      else {
        pal <- colorNumeric(palette = "PuBu",brasil_df[[input$radiomapabrasil]])
      }
    })
    
    palcolor <- reactive({
      pal <- pal()
      ~pal(brasil_df[[input$radiomapabrasil]]) 
    })
    #### Adicionar e retirar legenda - mapa do brasil
    observe({
      proxy <- leafletProxy("mapa_br_aux", data = brasil_df)
    
      if (input$radiomapabrasil == 'QUANTIDADE.DE.BENEFICIÁRIOS'){
        legenda_t <- 'Quantidade de Beneficiários'
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
                                    strong('Estado: '), name_state, "<br>",
                                    strong('Quantidade de Beneficiados: '), 
                                    formatC(
                                        QUANTIDADE.DE.BENEFICIÁRIOS,
                                        big.mark='.', format = 'fg',
                                        decimal.mark = ','
                                    ),"<br>",
                                    strong('Total Disponibilizado: '),
                                    formatC(
                                      TOTAL.DISPONIBILIZADO,
                                      big.mark='.', format = 'fg',
                                      decimal.mark = ','
                                    ), "<br>",
                                    strong('Percentual de Beneficiários (Diretamente): '),
                                    porcentagem_beneficiada, ' %' , "<br>"
                              )
                    )
    })

    
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
    })
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
        'Top 10 - '
        
      } 
      else {
        'Bottom 10 - '
      }
    })
    
    ##### Texto indicado qual variável está sendo utilizada
    texto_variavel <- reactive({
      if (input$top_or_bottom_choice == "QUANTIDADE DE BENEFICIÁRIOS"){
        'Quantidade de Beneficiários'
      } 
      else if (input$top_or_bottom_choice == "TOTAL DISPONIBILIZADO"){
        'Total Disponibilizado'
      }
      else {
        '% Beneficiada'
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
        geom_bar(aes(fill = .data[[input$top_or_bottom_choice]])) +
        coord_flip() +
        labs(
          title = paste(titlep1,titlep2),
          x = NULL,
          y = NULL
        ) +
        scale_fill_gradient(low='#1bece3',high ='#2f89a4') +
        scale_y_continuous(labels = label_comma(
          big.mark = '.',
          decimal.mark = ','
        )) +
        guides(fill=FALSE) +
        theme_minimal() +
        theme(text = element_text(family = 'titillium web'))
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
        subtitle = 'Percentual de Beneficiários (Diretamente)',
        color = 'teal',
        icon = icon('percentage')
      )
    })
    
    #### Média valor pago (mensal)
    media_valor_pago_mensal <- reactive({
      round(sum(brasil_df_raw$`TOTAL DISPONIBILIZADO`)/sum(brasil_df_raw$`QUANTIDADE DE BENEFICIÁRIOS`),0)/9
      
    })

# Dataset - Brasil --------------------------------------------------------
    
    ##### Filtrando dataset
    dataset_br <- brasil_df_raw %>%
      rename('Estado' = name_state,
             'Qtd de Beneficiários' = `QUANTIDADE DE BENEFICIÁRIOS`,
             'Total Disponibilizado' = `TOTAL DISPONIBILIZADO`,
             'População Total' = `População total`,
             '% de beneficiados' = `porcentagem_beneficiada`) %>%
      mutate(`Qtd de Beneficiários` = paste(comma(`Qtd de Beneficiários`,
                                                  big.mark = '.',
                                                  decimal.mark = ',')),
             `População Total` = paste(comma(`População Total`,
                                             big.mark = '.',
                                             decimal.mark = ',')),
             `Total Disponibilizado` = paste(sep = '','R$ ',comma(`Total Disponibilizado`,
                                                                  big.mark = '.',
                                                                  decimal.mark = ','),',00'))
    ##### Renderizar dataset
    output$databrasil <- DT::renderDT(server = FALSE,
                                      DT::datatable(dataset_br,
                                                    extensions = 'Buttons', options = list(
                                                      dom = 'Bfrtip',
                                                      buttons = 'csv'
                                                      )) 
                                      )


}
# Run the application 
shinyApp(ui = ui, server = server)

    