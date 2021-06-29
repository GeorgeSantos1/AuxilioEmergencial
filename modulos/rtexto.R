
options(encoding = "UTF-8")
bahia_df_raw <- qs::qread('Dataset/bahia_df_raw')
municipios_mapa <- qs::qread('Dataset/municipios_mapa')

nameui <- function(id){
  ns <- NS(id)
  tagList(
        fluidRow(
          box(
            status = 'primary',
            color="red",
            solidHeader = FALSE,
            width = 8,
            height = "98px",
            column(
              4,
              awesomeRadio(
                inputId = ns('todos_municipios'),
                label = 'Todos os Municípios?',
                choices = c("Sim", "Não"),
                selected = 'Sim',
                inline = FALSE,
                checkbox = TRUE
              )
            ),
            column(
              4,
              offset = 2,
              conditionalPanel(
                condition = "input.todos_municipios == 'Não'",
                ns = ns,
                selectInput(
                  inputId = ns('select_municipios'),
                  label = 'Escolha um Município: ',
                  choices = unique(bahia_df_raw$nome_munic),
                  selected = 'Salvador'
                )
              )
            )
          ),
          valueBoxOutput(ns('populacao_total_ba'),width = 4),
          valueBoxOutput(ns('total_pago_ba'),width = 3),
          valueBoxOutput(ns('media_beneficiados'),width = 3),
          valueBoxOutput(ns('porcentagem_beneficiados'),width = 3),
          valueBoxOutput(ns("media_valor_pago"),width = 3),
          box(
            status = 'primary',
            color="red",
            solidHeader = FALSE,
            width = 4,
            collapsible = TRUE,
            awesomeRadio(
              inputId = ns("radio_mapa_bahia"),
              label = 'Tematizar com:',
              choices = c('Total Disponibilizado' = "valor",
                          "Quantidade de Beneficiados" = "quantidadeBeneficiados",
                          "% da População Beneficiada" = "X..Beneficiados"),
              selected = "valor",
              status = "success"
            ),
            awesomeCheckbox(
              inputId = ns('exibir_legendas_bahia'),
              label = 'Mostrar Legenda?'
            )
          ),
          box(
            title = 'Distribuição do Auxílio Emergencial',
            status = 'primary',
            color="red",
            solidHeader = TRUE,
            width = 8,
            collapsible = TRUE,
            leafletOutput(ns('mapa_ba_aux'),height = 400) %>% 
              withSpinner(type=4,
                          color = '#40e0d0')
          ),
          column(
            width = 12,
            box(
              title = 'Principais Municípios Beneficiados',
              status = 'primary',
              solidHeader = TRUE,
              width = 8,
              collapsible = TRUE,
              plotlyOutput(ns('graf_top5')) %>% 
                withSpinner(type=4,
                            color = '#40e0d0')
            ),
            box(
              status = 'primary',
              solidHeader = FALSE,
              width = 4,
              collapsible = TRUE,
              awesomeRadio(
                inputId = ns('top_or_bottom_choice'),
                label = 'Selecione a Variável: ',
                choices = c('Quantidade de Beneficiados' = "quantidadeBeneficiados",
                            'Total Disponibilizado' = "valor",
                            '% da População Beneficiada' = '% Beneficiados'),
                selected = "quantidadeBeneficiados",
                status = 'success'
              ),
              tippy(
                div(
                  style = "text-align: center;",
                  switchInput(
                    inputId = ns("top_or_bottom"),
                    label = "Municípios com:",
                    value = TRUE,
                    onLabel = 'Maior Quantidade',
                    size = 'normal',
                    offLabel = 'Menor Quantidade',
                    labelWidth = "100px",
                    handleWidth = "100px",
                    offStatus = 'danger'
                  )
                ),
                tooltip = 'Clique Aqui Para Modificar o Gráfico'
              )
            )
          )
    )
  )
}

name <- function(input, output, session){
  ns <- session$ns

# Total Pago --------------------------------------------------------------
  total_pago_ba <- reactive({
    if (input$todos_municipios == 'Sim'){
      bahia_df_raw %>% 
        select(valor) %>%
        sum() %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',') %>%
                paste('R$ ',.,sep='')
    } else {
      bahia_df_raw %>%
        filter(nome_munic == input$select_municipios) %>%
        select(valor) %>%
        sum() %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',') %>%
        paste('R$ ',.,sep='')
    }
  })
  output$total_pago_ba <- renderValueBox({
    valueBox(
      value = h3(total_pago_ba(),class='value-box-h3',style='font-size: 25px;'),
      subtitle = 'Total Pago',
      color = 'teal',
      icon = icon('coins')
    )
  })

# População Total ---------------------------------------------------------
  populacao_total_ba <- reactive({
    if (input$todos_municipios == 'Sim'){
      bahia_df_raw %>% 
        select(Populacao) %>%
        sum() %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',')
    } else {
      bahia_df_raw %>%
        filter(nome_munic == input$select_municipios) %>%
        select(Populacao) %>%
        sum() %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',')
    }
  })
  output$populacao_total_ba <- renderValueBox({
    valueBox(
      value = h3(populacao_total_ba(),class='value-box-h3',style='font-size: 25px;'),
      subtitle = 'População (IBGE - 2020)',
      color = 'teal',
      icon = icon('users')
    )
  })

# média beneficiados ------------------------------------------------------
  media_beneficiados <- reactive({
    if (input$todos_municipios == 'Sim'){
      5827489 %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',')
    } else {
      bahia_df_raw %>%
        filter(nome_munic == input$select_municipios) %>%
        select(quantidadeBeneficiados) %>%
        sum() %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',')
    }
  })
  output$media_beneficiados <- renderValueBox({
    valueBox(
      value = h3(media_beneficiados(),class='value-box-h3',style='font-size: 25px;'),
      subtitle = 'Beneficiados (Média Mensal)',
      color = 'teal',
      icon = icon('user-check')
    )
  })

# % da população beneficiada ----------------------------------------------
  porcentagem_beneficiados <- reactive({
    if (input$todos_municipios == 'Sim'){
      paste("39",",","03"," %",sep='')
    } else {
      bahia_df_raw %>%
        filter(nome_munic == input$select_municipios) %>%
        select(`% Beneficiados`) %>%
        sum() %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',')
    }
  })
  output$porcentagem_beneficiados <- renderValueBox({
    valueBox(
      value = h3(porcentagem_beneficiados(),class='value-box-h3',style='font-size: 25px;'),
      subtitle = '% de Beneficiados (Diretamente)',
      color = 'teal',
      icon = icon('percentage')
    )
  })

# Média valor pago (ano) --------------------------------------------------
  media_valor_pago <- reactive({
    if (input$todos_municipios == 'Sim'){
      paste("R$ ","4.308,00",sep='')
    } else {
      bahia_df_raw %>%
        filter(nome_munic == input$select_municipios) %>%
        mutate(valor_anual = round((valor/quantidadeBeneficiados),0)) %>%
        select(valor_anual) %>%
        sum() %>%
        formatC(big.mark = '.',format = 'fg',decimal.mark = ',') %>%
        paste('R$ ',. ,',00',sep='')
    }
  })
  output$media_valor_pago <- renderValueBox({
    valueBox(
      value = h3(media_valor_pago(),class='value-box-h3',style='font-size: 25px;'),
      subtitle = 'Valor Médio Pago (Anual)',
      color = 'teal',
      icon = icon('credit-card')
    )
  })
  
  
  
# Mapa Bahia --------------------------------------------------------------
  palBA <- reactive({
    
    if (input$radio_mapa_bahia == 'valor'){
      risk.bins <- c(0,100000000,250000000,500000000,750000000,1000000000,5000000000)
      palBA <- colorBin(terrain.colors(9,rev = TRUE)[4:9], bins=risk.bins, na.color = "#aaff56")
    }
    else if (input$radio_mapa_bahia == 'quantidadeBeneficiados'){
      risk.bins <-c(0,15000,30000,45000,60000,75000,900000)
      palBA <- colorBin(terrain.colors(9,rev = TRUE)[4:9], bins=risk.bins, na.color = "#aaff56")
    }
    else if (input$radio_mapa_bahia == 'X..Beneficiados'){
      risk.bins <-c(0,25,30,35,40,45,50,55,60)
      colorBin(terrain.colors(9,rev = TRUE)[2:9], bins=risk.bins, na.color = "#aaff56")
    }
  })
  
  palcolorBA <- reactive({
    palBA <- palBA()
    ~palBA(municipios_mapa[[input$radio_mapa_bahia]]) 
  })
  
  labelsBA <- reactive({
    if (input$radio_mapa_bahia == 'valor'){
      c("0 - 100 Milhões","100 Milhões - 250 Milhões","250 Milhões - 500 Milhões",
        "500 Milhões - 750 Milhões","750 Milhões - 1 Bilhão", "Acima de 1 Bilhão") 
    }
    else if (input$radio_mapa_bahia == 'quantidadeBeneficiados'){
      c("0 - 15 Mil", "15 Mil - 30 Mil","30 Mil - 45 Mil","45 Mil - 60 Mil",
        "60 Mil - 75 Mil", "Acima de 75 Mil")
    }
    else if (input$radio_mapa_bahia == 'X..Beneficiados'){
      c("0 - 25%", "25% - 30%", "30% - 35%", "35% - 40%", "40% - 45%", "45% - 50%",
        "50% - 55%", "55% - 60%")
    }
  })
  
  output$mapa_ba_aux <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=municipios_mapa,
                  smoothFactor = 0.5,
                  fillOpacity = 0.4,
                  weight = 0.5,
                  opacity = 0.8,
                  label = ~name_muni,
                  color = palcolorBA(),
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~paste0(sep = " ",
                                  strong('Cidade: '), name_muni, "<br>",
                                  strong('Quantidade de Beneficiados: '), 
                                  formatC(
                                    quantidadeBeneficiados,
                                    big.mark='.', format = 'fg',
                                    decimal.mark = ','
                                  ),"<br>",
                                  strong('Total Disponibilizado: '),
                                  formatC(
                                    valor,
                                    big.mark='.', format = 'fg',
                                    decimal.mark = ','
                                  ), "<br>",
                                  strong('% de Beneficiados: '),
                                  X..Beneficiados, ' %' , "<br>"
                  )
      ) %>%
      setView(lat = -12.9704,lng = -38.5124,zoom = 6)
  })
  
  observe({
    proxy <- leafletProxy("mapa_ba_aux", data = municipios_mapa)
    
    if (input$radio_mapa_bahia == 'quantidadeBeneficiados'){
      legenda_t <- 'Quantidade de Beneficiados'
    } else if (input$radio_mapa_bahia == 'valor') {
      legenda_t <- 'Total Disponibilizado'
    } else {
      legenda_t <- '% Beneficiados'
    }
    proxy %>% clearControls()
    if (input$exibir_legendas_bahia){
      palBA <- palBA()
      labelsBA <- labelsBA()
      proxy %>%
        addLegend("bottomright",
                  title = legenda_t,
                  pal = palBA,
                  labFormat = function(type, cuts, p) {  # Here's the trick
                    paste0(labelsBA)
                  },
                  values = ~municipios_mapa[[input$radio_mapa_bahia]], 
                  opacity = 0.8)
    }
  })
  
  ##### Dados Filtrados - Gráfico Top/Bot 10
  data_br_filtrado <- reactive({
    
    if (input$top_or_bottom == TRUE){
      
      df <- bahia_df_raw %>% 
        select(nome_munic,.data[[input$top_or_bottom_choice]]) %>%
        arrange(desc(.data[[input$top_or_bottom_choice]])) %>%
        mutate(nome_munic = fct_reorder(nome_munic,
                                        .data[[input$top_or_bottom_choice]])) %>%
        head(10)
    }
    else {
      
      df <- bahia_df_raw %>% 
        select(nome_munic,.data[[input$top_or_bottom_choice]]) %>%
        arrange(.data[[input$top_or_bottom_choice]]) %>%
        mutate(nome_munic = fct_reorder(nome_munic,
                                        .data[[input$top_or_bottom_choice]]),
               nome_munic = fct_rev(nome_munic)) %>%
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
      'Municípios com maior '
      
    } 
    else {
      'Municípios com menor '
    }
  })
  
  ##### Texto indicado qual variável está sendo utilizada
  texto_variavel <- reactive({
    if (input$top_or_bottom_choice == "quantidadeBeneficiados"){
      'quantidade de beneficiados'
    } 
    else if (input$top_or_bottom_choice == "valor"){
      'total disponibilizado (R$)'
    }
    else {
      '% da população beneficiada'
    }
  })
  
  ##### Definição acurácia
  acuracia <- reactive({
    if (input$top_or_bottom_choice == '% Beneficiados'){
      0.1
    }
    else {
      NULL
    }
  })
  
  ##### Gráfico Top/Bot 10
  output$graf_top5 <- renderPlotly({
    acuracia <- acuracia()
    
    bahia_df_raw_filtrado <- data_br_filtrado()
    
    titlep1 <- top_or_bot_10()
    titlep2 <- texto_variavel()
    
    g2 <- ggplot(bahia_df_raw_filtrado) +
      aes(
        x = nome_munic,
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
      # guides(fill=FALSE,colour = FALSE,scale = "none") +
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
  


}

# Copy in UI
#nameui("nameui")

# Copy in server
#callModule(name, "nameui")