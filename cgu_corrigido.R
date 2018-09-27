##executivo federal template

setwd("C:/Users/jvoig/OneDrive/Documentos/relatorio_aEp_colab")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)
library(data.table)
library(readr)

como_data <- function(x) {
  
  stopifnot(require(dplyr))
  x <- gsub(" .*", "", x)
  y <- gsub(".*/", "", x)
  x <- if_else((nchar(y)==4), as.Date(x, format="%d/%m/%Y"),
               as.Date(x, format="%d/%m/%y"))
  
}


cgu_original  <- read_csv("C:/Users/jvoig/OneDrive/Documentos/relatorio_aEp_colab/cgu_final_03092018.xlsx - Folha1.csv", col_types = cols(.default = "c"))

cgu <- cgu_original %>%
  mutate(rem_url = ifelse(grepl("^\\&lt.*", resposta), 1, 0)) %>%
  filter(rem_url == 0) %>%
  clean_names() %>%
  gather(interacao, conteudo, c(pedido, resposta, recurso_1,
                                resposta_recurso_1, recurso_2, resposta_recurso_2,
                                recurso_3, resposta_recurso_3, recurso_4, resposta_recurso_4)) %>%
  filter(is.na(nao_e_pedido_de_informacao),
         !is.na(conteudo)) %>%
  mutate(data = ifelse(interacao == "pedido", data_do_pedido,
                       ifelse(interacao == "resposta", data_da_resposta,
                              ifelse(interacao == "recurso_1", data_recurso_1,
                                     ifelse(interacao == "resposta_recurso_1", data_resposta_recurso_1,
                                            ifelse(interacao == "recurso_2", data_recurso_2,
                                                   ifelse(interacao == "resposta_recurso_2", data_resposta_recurso_2,
                                                          ifelse(interacao == "recurso_3", data_recurso_3,
                                                                 ifelse(interacao == "resposta_recurso_3", data_resposta_recurso_3,
                                                                        ifelse(interacao == "recurso_4", data_recurso_4,
                                                                               ifelse(interacao == "resposta_recurso_4", data_resposta_recurso_4, NA))))))))))) %>%
  mutate(PastaAnexos = pasta_do_anexo_resposta) %>%
  mutate(interacao = gsub("pedido", "Pedido", interacao),
         interacao = gsub("\\bresposta\\b", "Resposta do Pedido", interacao),
         interacao = gsub("\\brecurso_1\\b", "Recurso - 1º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_1\\b", "Resposta do Recurso - 1º Instância", interacao),
         interacao = gsub("\\brecurso_2\\b", "Recurso - 2º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_2\\b", "Resposta do Recurso - 2º Instância", interacao),
         interacao = gsub("\\brecurso_3\\b", "Recurso - 3º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_3\\b", "Resposta do Recurso - 3º Instância", interacao)) %>%
  mutate(UF = NA,
         municipio = NA,
         prorrogado = NA,
         id = NA,
         titulo = orgao,
         situacao = "Finalizado",
         NomeAnexos = NA) %>%
  mutate(data2 = como_data(data),
         data3 = as.Date(data, format="%Y-%m-%d"),
         data4 = if_else(!is.na(data3), data3, data2)) %>%
  select(id, protocolo, titulo, conteudo, interacao, data4, prorrogado, orgao, atendimento,
         situacao, UF, municipio, poder, esfera, PastaAnexos, NomeAnexos, assunto, tipo_destino, destino) %>%
  rename(data = data4)


# cgu %>%
#   mutate(original = ifelse(is.na(data), 1, 0),
#          fix = ifelse(is.na(data4), 1, 0)) %>%
#   summarise(original = sum(original),
#             fix = sum(fix))

write.xlsx(as.data.frame(cgu), file="cgu_corrigido.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(cgu, file = "cgu_corrigido.RData")

