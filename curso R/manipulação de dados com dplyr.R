library(AER)
library(tidyverse)

data("SwissLabor")

dados <- SwissLabor

# Filter -----------------------------------------------------------------------

dados %>% 
  filter(participation == "yes")

## Podemos também criar filtros baseados em múltiplas condições

dados %>%
  filter(education > 8 & age < 5)

# arrange ----------------------------------------------------------------------

dados %>%
  arrange(income)

## por default a função arrange faz ordenação crescente. Se quisermos ordenação 
## decrescente podemos fazer

dados %>% 
  arrange(desc(income))

## Podemos também ordenar por mais de uma variável 

dados %>% 
  arrange(education, youngkids)

# distinct ---------------------------------------------------------------------

## No exemplo abaixo queremos identificar todos os pares de 'youngkids' e 'oldkids'
## distintos

dados %>% 
  distinct(youngkids, oldkids)
