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

# Combinando mais de uma operação em único pipeline ----------------------------

dados %>% 
  filter(income < 10000) %>%
  arrange(age)

## Codigo equivalente sem usar dplyr e %>% 

dados_ordenados <- dados[order(dados$age), ][dados$income < 10000, ]

#  mutate ----------------------------------------------------------------------

dados %>% 
  mutate(total_kids = youngkids + oldkids)

# se quisermos especificar a posição da nova coluna podemos usar .before ou. after

dados %>% 
  mutate(total_kids = youngkids + oldkids,
         .before = foreign)

# select -----------------------------------------------------------------------

dados %>% 
  select(participation, income, age)

## se quisermos apenas variáveis numéricas -------------------------------------

dados %>% 
    select(where(is.numeric))

## se quisermos apenas as variáveis que contém a palavra kids ------------------

dados %>% 
  select(contains("kids"))

# rename -----------------------------------------------------------------------

dados %>% 
  rename(criancasjovens = youngkids)

# relocate ---------------------------------------------------------------------

dados %>% 
  relocate(income, age)

dados %>%
  relocate(age, 
           .before = income)

# group_by ---------------------------------------------------------------------


