library(AER)
library(tidyverse)
library(openxlsx)

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

dados %>% 
  group_by(participation)

## Note que não houve nenhuma alteração no dataset. Todas as operações subsequentes
## serão realizadas sobre os dois grupos criados. 

dados %>% 
  group_by(participation) %>% 
  summarise(mean_income = mean(income),
            n = n())

## Neste caso, observamos que a renda média dos indivíduos que participam do mer-
## cado de trabalho é 10,6 e dos que não participam, 10,8.

# Agrupamento por múltiplas colunas --------------------------------------------

dados %>% 
  group_by(participation, foreign) %>% 
  summarise(mean_income = mean(income),
            n = n())

# ungroup ----------------------------------------------------------------------

dados %>% 
  group_by(participation) %>% 
  ungroup() %>% 
  summarise(mean_income = mean(income),
            n = n())

# .by --------------------------------------------------------------------------

dados %>% 
  summarise(mean_income = mean(income),
            n = n(),
            .by = participation)

# slice ------------------------------------------------------------------------

dados %>% 
  group_by(participation) %>% 
  slice(1) # seleciona a primeira linha de cada grupo

dados %>% 
  group_by(participation) %>% 
  slice_head(n = 5) # seleciona as cinco primeiras linhas de cada grupo

# join -------------------------------------------------------------------------

df1 <- data.frame(ID = c(1, 2, 3, 4),
                  Nome = c("Ana", "Bruno", "Carlos", "Daniel"))

df2 <- data.frame(ID = c(3, 4, 5, 6),
                  Cidade = c("São Paulo", "Rio de Janeiro", "Curitiba", "Salvador"))

resultado_inner <- inner_join(df1, df2, by = "ID")

resultado_left <- left_join(df1, df2, by = "ID")

resultado_right <- right_join(df1, df2, by = "ID")

resultado_full <- full_join(df1, df2, by = "ID")


# Exibindo os resultados
resultado_inner
resultado_left
resultado_right
resultado_full

novo_swiss_labor <- dados %>% 
  mutate(total_kids = youngkids + oldkids)

write.xlsx(novo_swiss_labor, file = "novo_swiss_labor.xlsx")

write.csv(novo_swiss_labor, file = "novo_swiss_labor.csv")
