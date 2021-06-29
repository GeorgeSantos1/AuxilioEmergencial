# 
# # Pacotes Carregados
# 
# library(jsonlite)
# library(httr)
# library(dplyr)
# 
# # Carregando informações adicionais
#   # * Meses de referência
#   # * Codigos IBGE dos municípios do estado da Bahia
#   # * Nomes dos municípios com acentuação correta
# 
# setwd("~/Auxilio_Aplication")
# source('Scripts/API_DF/auxiliar.r')
# 
# # Separando parâmetros
# 
# base_url <- "http://api.portaldatransparencia.gov.br/api-de-dados/auxilio-emergencial-por-municipio"
# codigoIBGE <- IBGE_BA[1]
# mesAno <- MES_ANO[1]
# pagina <- 1
# 
# # fazendo primeiro request para criação de colunas do dataframe
# res_api <- GET(
#   url = base_url,
#   add_headers("chave-api-dados"="ca949fbe985769297c950bab666935c1"),
#   query = list(
#     codigoIbge= codigoIBGE,
#     mesAno = mesAno,
#     pagina = pagina)
#   )
# 
# content_text <- rawToChar(res_api$content)
# df <- fromJSON(content_text, flatten=TRUE)
# df[1,] <- NA
# 
# # Loop para geração do dataframe (~ 25 min, limitação de 100 requisições por minuto)
# for (i in 1:length(IBGE_BA)) {
#   for (j in 1:length(MES_ANO)) {
#     codigoIBGE <- IBGE_BA[i]
#     mesAno <- MES_ANO[j]
#     res_api <- GET(
#       url = base_url,
#       add_headers("chave-api-dados"="ca949fbe985769297c950bab666935c1"),
#       query = list(
#         codigoIbge= codigoIBGE,
#         mesAno = mesAno,
#         pagina = pagina)
#     )
#     content_text <- rawToChar(res_api$content)
#     content_json <- fromJSON(content_text, flatten=TRUE)
#     df <- rbind(df,content_json)
#     Sys.sleep(0.7)
#   }
# }
# 
# # Descartando colunas desnecessárias
# df1 <- df[-1,] %>%
#   select(-c(municipio.nomeIBGE,
#             tipo.descricao,
#             tipo.descricaoDetalhada,
#             municipio.codigoRegiao,
#             municipio.pais,
#             municipio.uf.sigla,
#             tipo.id))
# 
# # Merge das informações necessárias com o dataframe processado
# df1 <- inner_join(df1,muni_names,by='municipio.codigoIBGE')
# 
# # Salvando dataframe em arquivo .csv
# write.csv2(df1,'Dataset/df.csv')
# 
