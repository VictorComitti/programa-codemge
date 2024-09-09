library(AER)
library(tidyverse)
library(openxlsx)

data("SwissLabor")

dados <- SwissLabor

# Filter -----------------------------------------------------------------------

dados %>% 
  filter(foreign == "yes") 

## Podemos também criar filtros baseados em múltiplas condições

dados %>%
  filter(education == 8 | education == 9)

# arrange ----------------------------------------------------------------------

dados %>%
  arrange(income) %>% View()

## por default a função arrange faz ordenação crescente. Se quisermos ordenação 
## decrescente podemos fazer

dados %>% 
  arrange(desc(income))

## Podemos também ordenar por mais de uma variável 

dados %>% 
  arrange(education, youngkids) %>% View()

# distinct ---------------------------------------------------------------------

## No exemplo abaixo queremos identificar todos os pares de 'youngkids' e 'oldkids'
## distintos

dados %>%
  distinct() %>% nrow()

dados %>% 
  distinct(education, youngkids)

# Combinando mais de uma operação em único pipeline ----------------------------

dados %>% 
  filter(income < 10) %>%
  arrange(age) %>% 
  View()

## Codigo equivalente sem usar dplyr e %>% 

dados_ordenados <- dados[order(dados$age), ][dados$income < 10, ]

dados_filtrados <- dados[dados$income < 10, ]
dados_ordenados <- dados_filtrados[order(dados_filtrados$age), ]

#  mutate ----------------------------------------------------------------------

dados %>% 
  mutate(total_kids = youngkids + oldkids) %>% View

# se quisermos especificar a posição da nova coluna podemos usar .before ou .after

dados %>% 
  mutate(total_kids = youngkids + oldkids,
         .after = education) %>% 
  View()

# select -----------------------------------------------------------------------

dados %>% 
  select(participation, income, age) %>% 
  View

## se quisermos apenas variáveis numéricas -------------------------------------

dados %>% 
  select(where(is.numeric)) %>%
  View()

## se quisermos apenas as variáveis que contém a palavra kids ------------------

dados %>% 
  select(contains("kids")) %>%
  View

# rename -----------------------------------------------------------------------

dados %>% 
  rename(criancasjovens = youngkids,
         criancasvelhas = oldkids)

# relocate ---------------------------------------------------------------------

dados %>% 
  relocate(income, age) %>% View()

dados %>%
  relocate(age, 
           .after = education) %>% View

# group_by ---------------------------------------------------------------------

dados %>% 
  group_by(participation) %>% View()

## Note que não houve nenhuma alteração no dataset. Todas as operações subsequentes
## serão realizadas sobre os dois grupos criados. 

dados %>% 
  group_by(participation) %>% 
  summarise(mean_income = mean(income),
            n = n()) %>% View()

dados %>%
  group_by(foreign) %>% 
  summarize(max_age = max(age))

## Neste caso, observamos que a renda média dos indivíduos que participam do mer-
## cado de trabalho é 10,6 e dos que não participam, 10,8.

# Agrupamento por múltiplas colunas --------------------------------------------

dados %>% 
  group_by(oldkids, foreign) %>% 
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
            .by = participation) %>% View()

# slice ------------------------------------------------------------------------

dados %>% 
  group_by(participation) %>% 
  slice(450) # seleciona a primeira linha de cada grupo

dados %>% 
  group_by(participation) %>% 
  slice_tail(n = 5) # seleciona as cinco primeiras linhas de cada grupo

dados %>% 
  slice_max(age)

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

# Salvando dados em R ----------------------------------------------------------

novo_swiss_labor <- dados %>% 
  mutate(total_kids = youngkids + oldkids)

write.xlsx(novo_swiss_labor, file = "novo_swiss_labor.xlsx")

write.csv(novo_swiss_labor, file = "novo_swiss_labor.csv")

getwd()
