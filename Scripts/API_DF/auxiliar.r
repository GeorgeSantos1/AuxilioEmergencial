# 
# library(dplyr)
# library(ribge)
# 
# IBGE_BA <-ribge::municipioIbgeTseMap %>%
#   dplyr::select(uf,cod_municipio) %>%
#   dplyr::filter(uf == 'BA') %>%
#   dplyr::select(cod_municipio) %>%
#   as.matrix()
# 
# MES_ANO <- c(paste(2020,0,seq(4,9),sep = ''),paste(2020,seq(10,12),sep=''))
# 
# muni_names <- ribge::censo2010MunicHabitantes %>%
#   dplyr::mutate(codigo_munic = paste(codigo_uf,codigo_munic,sep='')) %>%
#   select(uf,codigo_munic,nome_munic) %>%
#   dplyr::filter(uf == 'BA') %>%
#   select(-uf) %>%
#   rename('municipio.codigoIBGE'='codigo_munic')
# 
# 
