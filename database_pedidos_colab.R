#juntando todos os amigos.

library(dplyr)
library(readxl)
library(readr)
library(googlesheets)
library(janitor)

gs_ls()

#tudo o que eu já tinha juntado:
load("C:/Users/jvoig/OneDrive/Documentos/relatorio_achados/database_ped_sem_cgu.Rdata")

#cgu
load("C:/Users/jvoig/OneDrive/Documentos/relatorio_aEp_colab/cgu_corrigido.RData")

#macrotemas 

sheet <- gs_title("macrotemas_colab")
macrotemas <- gs_read(sheet)

macrotemas <- macrotemas %>% 
  clean_names() %>%
  select(1:4) %>%
  mutate(poder = ifelse(poder == "Tribunal de Contas",  "Tribunal de Contas", poder),
         poder = ifelse(poder == "Ministério Público",  "Ministério Público", poder),
         esfera = stringr::str_to_title(esfera))


database_pedidos_colab <- database_ped_sem_cgu %>%
  bind_rows(cgu) %>%
  mutate(esfera = ifelse(esfera == "Distrital", "Estadual", esfera)) %>%
  left_join(macrotemas, by=c("poder", "esfera", "assunto"))


database_pedidos_colab %>%
  mutate(num = ifelse(is.na(macrotema), 1, 0)) %>%
  group_by(poder, esfera) %>%
  summarise(n(),
            sum(num))


write.xlsx(as.data.frame(database_pedidos_colab), file="database_pedidos_colab.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.csv(database_pedidos_colab, file="database_pedidos_colab.csv", fileEncoding = "UTF-8")

save(database_pedidos_colab, file = "database_pedidos_colab.RData")