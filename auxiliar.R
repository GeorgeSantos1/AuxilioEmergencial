

# Pacotes Utilizados ------------------------------------------------------

library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(stringr)
library(sf)
library(rmapshaper)

# Carregando Bases de Dados -----------------------------------------------

brasil_df <- read_csv("Dataset/2020_beneficios_estado.csv",
                      col_types = cols(LOCALIDADE = col_character()))
## Para baixar os dados geoespaciais:
  #* estados <- geobr::read_state(year = '2020')
  #* saveRDS(estados,'Dataset/estados.rds')

estados <- readRDS('Dataset/estados.rds')

brasil_df <- brasil_df %>%
  mutate(LOCALIDADE = str_replace_all(brasil_df$LOCALIDADE,
                                                 c('Amazonas' = "Amazônas",
                                                   "Mato Grosso do Sul" = 'Mato Grosso Do Sul',
                                                   "Rio de Janeiro" = 'Rio De Janeiro',
                                                   "Rio Grande do Norte" = 'Rio Grande Do Norte',
                                                   "Rio Grande do Sul" = 'Rio Grande Do Sul'))) %>%
  rename('name_state'='LOCALIDADE')

brasil_df_raw <- brasil_df
brasil_df <- inner_join(estados,brasil_df,by='name_state')

brasil_df <- sf::as_Spatial(brasil_df)

## Para simplificar os dados geoespaciais 
  #* brasil_df <- rmapshaper::ms_simplify(brasil_df, keep = 0.05, keep_shapes = TRUE)
  #* saveRDS(brasil_df,'Dataset/estados_simplificados.rds')

brasil_df <- readRDS('Dataset/estados_simplificados.rds')
rm(estados)

