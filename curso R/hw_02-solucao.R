rm(list =)

library(tidyverse)
library(openxlsx)

dados <- read.xlsx("hw_02.xlsx")

valores_recebidos <- dados %>%
  group_by(id_venda) %>%
  summarise(valor_recebido = sum(valor_recebido)) %>%
  mutate(valor_recebido = ifelse(is.na(valor_recebido),
                                       0, valor_recebido))

valores_recebidos <- dados %>%
  filter(categoria_produto == "Alto Padrão") %>%
  group_by(id_venda) %>%
  summarise(valor_recebido = sum(valor_recebido, na.rm = TRUE)) 

dados %>% 
  group_by(id_venda) %>% 
  summarise(valor_venda = first(valor_venda),
            parcelas = n(), 
            parcelas_quitadas = sum(valor_recebido > 0, na.rm = TRUE)) %>% 
  View
