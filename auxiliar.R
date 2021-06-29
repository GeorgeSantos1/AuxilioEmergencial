

# # Pacotes Utilizados ------------------------------------------------------
# 
# library(dplyr)
# library(magrittr)
# library(readr)
# library(ggplot2)
# library(stringr)
# library(rmapshaper)
# library(ribge)
# library(rvest)
# library(httr)

# Carregando Bases de Dados -----------------------------------------------

# brasil_df <- read_csv("Dataset/2020_beneficios_estado.csv",
#                        col_types = cols(LOCALIDADE = col_character()))

## Para baixar os dados geoespaciais:
  #* estados <- geobr::read_state(year = '2020')
  #* saveRDS(estados,'Dataset/estados.rds')

## Para baixar população dos municipios
  #* url <- 'https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_popula%C3%A7%C3%A3o_(2020)'
  #* html <- read_html(url)
  #* aa <- html %>% rvest::html_nodes('.wikitable')
  #* aa <- aa[[1]] %>% rvest::html_table()
  #* saveRDS(aa,'Dataset/pop_estimativa_2020.rds')

# estados <- readRDS('Dataset/estados.rds')
# pop_2020 <- readRDS('Dataset/pop_estimativa_2020.rds')

# pop_2020_reg <- pop_2020 %>%
#    mutate(
#      `Unidade federativa` = str_replace_all(`Unidade federativa`,
#                                             c('Amazonas' = "Amazônas",
#                                               "Mato Grosso do Sul" = 'Mato Grosso Do Sul',
#                                               "Rio de Janeiro" = 'Rio De Janeiro',
#                                               "Rio Grande do Norte" = 'Rio Grande Do Norte',
#                                               "Rio Grande do Sul" = 'Rio Grande Do Sul')),
#      População = tidyr::extract_numeric(População)
#      ) %>%
#    group_by(`Unidade federativa`) %>%
#    summarise(`População total` = sum(População),.groups = 'drop') %>%
#    rename('name_state'='Unidade federativa')

# brasil_df <- brasil_df %>%
#   mutate(LOCALIDADE = str_replace_all(LOCALIDADE,
#                                                  c('Amazonas' = "Amazônas",
#                                                    "Mato Grosso do Sul" = 'Mato Grosso Do Sul',
#                                                    "Rio de Janeiro" = 'Rio De Janeiro',
#                                                    "Rio Grande do Norte" = 'Rio Grande Do Norte',
#                                                    "Rio Grande do Sul" = 'Rio Grande Do Sul'))) %>%
#   rename('name_state'='LOCALIDADE')

# brasil_df_raw <- brasil_df
# brasil_df <- inner_join(estados,brasil_df,by='name_state')
# brasil_df <- inner_join(brasil_df,pop_2020_reg,by='name_state')

# brasil_df_raw <- inner_join(brasil_df_raw,pop_2020_reg,by='name_state')

# brasil_df <- sf::as_Spatial(brasil_df)

## Para simplificar os dados geoespaciais 
  #* brasil_df <- rmapshaper::ms_simplify(brasil_df, keep = 0.05, keep_shapes = TRUE)
  #* saveRDS(brasil_df,'Dataset/estados_simplificados.rds')

## Modificando base de dados
# brasil_df$porcentagem_beneficiada <- round(
#   (brasil_df$QUANTIDADE.DE.BENEFICIÁRIOS/brasil_df$População.total)*100,
#   digits = 2
# )
# brasil_df_raw$porcentagem_beneficiada <- round(
#   (brasil_df_raw$`QUANTIDADE DE BENEFICIÁRIOS`/brasil_df_raw$`População total`)*100,
#   digits = 2
# )

## Brasil Banco Raw
  #* saveRDS(brasil_df_raw,'Dataset/brasil_df_raw.rds')
  #* brasil_df <- readRDS('Dataset/estados_simplificados.rds')
  #* brasil_df_raw <- readRDS('Dataset/brasil_df_raw.rds')
# qs::qsave(brasil_df,'Dataset/estados_simplificados')
# qs::qsave(brasil_df_raw,'Dataset/brasil_df_raw')

brasil_df <- qs::qread('Dataset/estados_simplificados')
brasil_df_raw <- qs::qread('Dataset/brasil_df_raw')

#######

# f <- function(data){
#   stringr::str_split(data,'-')[[1]][2]
# }
# 
# bahia_df_raw <- readxl::read_xlsx('Dataset/df.xlsx')
# bahia_df_raw <- bahia_df_raw %>%
#   mutate(mes = (purrr::map(dataReferencia,f)),
#          mes = lubridate::month(as.numeric(mes), 
#                                 label = TRUE, 
#                                 locale = Sys.getlocale("LC_TIME")))
# 
# pop_2020 <- readRDS('Dataset/pop_estimativa_2020.rds')
# 
# bahia_df_raw <- inner_join(bahia_df_raw,pop_2020[,c(2,5)],by=c("municipio.codigoIBGE"="Código IBGE"))
# bahia_df_raw <- bahia_df_raw %>%
#   mutate(População = stringr::str_replace_all(População,"[[:space:]]+",""))
# 
# bahia_df_raw <- bahia_df_raw %>%
#   group_by(nome_munic) %>%
#   summarise(valor = sum(valor),
#             quantidadeBeneficiados = mean(quantidadeBeneficiados) %>% round(0),
#             População = as.numeric(min(População)),
#             municipio.codigoIBGE = as.numeric(min(municipio.codigoIBGE))) %>%
#   mutate("% Beneficiados" = round(quantidadeBeneficiados/População * 100,2))

# municipios <- geobr::read_municipality(code_muni = "BA",year = '2020')
# 
# municipios_mapa <- inner_join(municipios,bahia_df_raw,by=c("code_muni"="municipio.codigoIBGE"))
# 
# municipios_mapa <- sf::as_Spatial(municipios_mapa)

## Para simplificar os dados geoespaciais
# municipios_mapa <- rmapshaper::ms_simplify(municipios_mapa, keep = 0.05, keep_shapes = TRUE)


# bahia_df_raw$População[bahia_df_raw$nome_munic == "Maetinga"] <- 7038
# bahia_df_raw$`% Beneficiados`[bahia_df_raw$nome_munic == "Maetinga"] <- 38.58
# 
# municipios_mapa$População[municipios_mapa$nome_munic == "Maetinga"] <- 7038
# municipios_mapa$X..Beneficiados[municipios_mapa$nome_munic == "Maetinga"] <- 38.58
# 
# qs::qsave(bahia_df_raw,'Dataset/bahia_df_raw')
# qs::qsave(municipios_mapa,'Dataset/municipios_mapa')

bahia_df_raw <- qs::qread('Dataset/bahia_df_raw')
municipios_mapa <- qs::qread('Dataset/municipios_mapa')


# names(brasil_df)[6] <- "QUANTIDADE_DE_BENEFICIARIOS"
# names(bahia_df_raw)[4] <- "Populacao"
