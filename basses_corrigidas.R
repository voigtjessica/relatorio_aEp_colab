#importando todas as bases de dados, limpando ,ajeitando as data e colocando no template.

library(dplyr)
library(janitor)
library(plyr)
library(googlesheets)

googlesheets::gs_ls()

bases_corrigidas <- list()
arq <- c("tcs_final_05062018.xlsx", "mp_final_18062018", "leg_final_18062018", "jud_final_11062018",
         "exec_municipal_final_250618", "exec_estadual_final_01072018.xlsx", "cgu_final_03092018.xlsx")

for(i in seq_along(arq)){
  print(i)
  sheet <- gs_title(arq[i])
  x <- gs_read(sheet)
  bases_corrigidas[[i]] <- x
}

#Descobrindo quantos não são pedidos de acesso à informação.

nao_e_pedido <- list()

for(i in 1:7){
  print(i)
  
  x <- bases_corrigidas[[i]]
  y <- names(x)
  
  
  if(sum(grepl("resposta_duplicada", y)==1)){
    x <- x %>%
      filter(is.na(resposta_duplicada)) 
    }
  
  x <- x %>%
    distinct(protocolo, .keep_all = TRUE) %>%
    mutate(protocolo = as.character(protocolo), 
           nao_e_pedido_de_informacao = as.numeric(nao_e_pedido_de_informacao),
           nome_arq = arq[i]) %>%
    select(protocolo, nao_e_pedido_de_informacao, nome_arq)

  nao_e_pedido[[i]] <- x
}

nao_e_pedido <- bind_rows(nao_e_pedido)

nao_e_pedido %>%
  mutate(nao_e_pedido_de_informacao = ifelse(is.na(nao_e_pedido_de_informacao), 0, nao_e_pedido_de_informacao),
         nao_e_pedido_de_informacao = as.numeric(nao_e_pedido_de_informacao)) %>%
  summarise(nao_era_pedido = sum(nao_e_pedido_de_informacao, na.rm=TRUE)) 

#910 não são pedido de informação.

######
# Agora vou baixar os arquivos que já estão no template:




