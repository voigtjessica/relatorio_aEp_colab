library(dplyr)
library(janitor)

setwd("C:/Users/jvoig/OneDrive/Documentos/colab_tb/fixing_data")

evtest <- function(base1, base2) {
  
  x <- names(base1)
  y <- names(base2)
  n <- length(x)
  k <- length(y)
  
  teste_nome_igual_x <- numeric()
  teste_nome_igual_y <- numeric()
  
  for ( i in 1:n) {
    teste_nome_igual_x[i] <- x[i] %in% y
  }
  
  for ( i in 1:k) {
    teste_nome_igual_y[i] <- y[i] %in% x
  }
  resp_x <- paste(x[!as.logical(teste_nome_igual_x)], collapse = ", ")
  resp_y <- paste(y[!as.logical(teste_nome_igual_y)], collapse = ", ")
  
  cat(paste("Colunas de", deparse(substitute(base1)), "ausentes em" , 
              deparse(substitute(base2)), ":", resp_x,
              ".\n\nColunas de", deparse(substitute(base2)), "ausentes em" ,
              deparse(substitute(base1)), ":", resp_y,
              sep=" "))
  
}

#loadando os tmpl:

load("execest_fixed.Rdata")

execest_fixed <- execest_fixed %>%
  clean_names() %>%
  select(-fix)

load("execum_fixed_semdup.Rdata")

ind <- ind %>%
  clean_names()

load("jud_fix.Rdata")

jud3 <- jud3 %>%
  clean_names() %>%
  rename(atendimento = status,
         esfera = nivel)

load("leg_fixed.Rdata")

leg_fixed <- leg_fixed %>%
  clean_names() %>%
  rename(assunto = assunto_final_beta,
         nome_anexos = nome_anexos_f)
  
load("mp_fix.Rdata")

mp_fix <- mp_fix %>%
  clean_names() %>%
  rename(assunto = assunto_final_beta)

load("tcsf_fix.Rdata")

tcsf_fix <- tcsf_fix %>%
  clean_names() %>%
  rename(pasta_anexos = pasta_anexos_f,
         assunto = assunto_final_beta)

evtest(ind, tcsf_fix)
#tudo ok

#juntando tudo:

database_ped_sem_cgu <- execest_fixed %>%
  bind_rows(ind, jud3, leg_fixed, mp_fix, tcsf_fix) %>%
  mutate(id = if_else(interacao == "Pedido", 1,
                      if_else(interacao == "Resposta do Pedido", 2,
                      if_else(interacao == "Recurso - 1º Instância", 3,
                      if_else(interacao == "Resposta do Recurso - 1º Instância", 4,
                      if_else(interacao == "Recurso - 2º Instância", 5,
                      if_else(interacao == "Resposta do Recurso - 2º Instância", 6,
                      if_else(interacao == "Recurso - 3º Instância", 7,
                      8))))))))

#Fazendo umas pequenas correções que fui vendo:
database_ped_sem_cgu <- database_ped_sem_cgu %>%
  mutate(esfera = if_else(orgao == "Superior Tribunal de Justiça", "Federal", esfera ))


setwd("C:/Users/jvoig/OneDrive/Documentos/relatorio_achados")
save(database_ped_sem_cgu, file="database_ped_sem_cgu.Rdata")
