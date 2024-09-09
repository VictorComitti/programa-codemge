library(car)
library(dplyr)
library(skimr)
library(ggplot2)
library(gridExtra)
library(corrplot)


# Vamos usar o conjunto de dados Chile que já faz parte da library 'car'--------

# A função str mostra a estrutura básica dos dados -----------------------------

str(Chile)

summary(Chile)

# Outra opção é utilizar a função skim do pacote skimr

skim(Chile)

# Note que a função skim já nos mostra além da estrutura dos dados, um sumário
# descritivo das variáveis que inclui a contagem de valores únicos, os quantis
# da distribuição das variáveis numéricas e o número de observações faltantes
# em cada variável.


chile_skim <-
  skim(Chile)

# O objeto skim pode ser manipulado usando o operador pipe %>%. Isso é bastante útil
# e nos permite, por exemplo, examinar as variáveis separadamente. Por exemplo:
# vamos filtrar apenas a variável sex no conjunto de dados original

chile_skim %>%
  filter(skim_variable == "sex")

# Podemos ainda selecionar apenas as informações de sumário que queremos ver. Exemplo:

chile_skim %>%
dplyr::select(skim_type, skim_variable, n_missing)

# Outra possibilidade é calcular os sumários descritivos por grupos. No exemplo abaixo,
# vamos agrupar os dados por sexo e calcular as estatísticas descritivas.

Chile %>%
  group_by(sex) %>%
  skim()


# Visualizando as variáveis cuja classe é factor. Temos que examinar region, sex,
# education, e vote

barplot_sex <- ggplot(Chile, aes(x = sex)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuição por sexo",
       x = "Sexo",
       y = "Contagem") +
  theme_minimal()

barplot_region <- ggplot(Chile, aes(x = region)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuição por Região",
       x = "Região",
       y = "Contagem") +
  theme_minimal()

barplot_education <- ggplot(Chile, aes(x = education)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuição por Educação",
       x = "Nível de Educação",
       y = "Contagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barplot_vote <- ggplot(Chile, aes(x = vote)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuição por Voto",
       x = "Vote",
       y = "Contagem") +
  theme_minimal()

# Organize os gráficos em um único painel
grid.arrange(barplot_sex, barplot_region, barplot_education,
             barplot_vote, ncol = 2)

# Agora vamos examinar as variáveis numéricas: population, age, income, statusquo

hist_population <- ggplot(Chile, aes(x = population)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histograma de População",
       x = "População",
       y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist_age <- ggplot(Chile, aes(x = age)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histograma de Idade",
       x = "Idade",
       y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist_income <- ggplot(Chile, aes(x = income)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histograma de Renda",
       x = "Renda",
       y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

hist_statusquo <- ggplot(Chile, aes(x = statusquo)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histograma de Status Quo",
       x = "Status Quo",
       y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Organize os histogramas em um único painel
grid.arrange(hist_population, hist_age, hist_income, hist_statusquo, ncol = 2)

# Note que as variáveis population e income, ainda que numéricas, na prática,
# só assumem um número finito de valores. Sendo assim, talvez um barplot seja mais
# adequado para visualizarmos esses dados

chile_skim %>%
  filter(skim_variable == "population")

table(Chile$population)

barplot_population <- ggplot(Chile, aes(x = as.factor(population))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuição por população",
       x = "Nível de Educação",
       y = "Contagem") +
  theme_minimal()

barplot_income <- ggplot(Chile, aes(x = as.factor(income))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuição por renda",
       x = "Vote",
       y = "Contagem") +
  theme_minimal()

# Box plots -------------------------------------------------------------------

# Outro gráfico útil para a visualização de variávies numéricas é o box-plot

boxplot_statusquo <- ggplot(Chile, aes(x = statusquo, y = statusquo)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de Status Quo",
       x = "Status Quo",
       y = "Valores") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot_age <- ggplot(Chile, aes(x = age, y = age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de Idade",
       x = "Idade",
       y = "Valores") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(boxplot_statusquo, boxplot_age, ncol = 2)

# Correlação entre as variáveis

variaveis_numericas <- Chile[, sapply(Chile, is.numeric)]

# Calcule a matriz de correlação
matriz_correlacao <- cor(variaveis_numericas,
                         use = "complete.obs") # Como as variáveis age, income e
                                               # statusquo possuem NAs, precisamos
                                               # usar a opção use = "complete.obs"
                                               # para selecionar apenas observações (linhas)
                                               # com valores preenchidos

# Visualize a matriz de correlação

corrplot(matriz_correlacao, method="color",
         addCoef.col = "black")#para essa função funcionar o
                                              # pacote corrplot deve estar carregado

# outliers ---------------------------------------------------------------------

 # Uma boa maneira de detectar observações discrepantes é através do box-plot. Podem
 # ser considerados outliers observações cujo valor esteja acima ou abaixo dos limites
 # teóricos da distribuição dos dados. No caso do banco de dados Chile, todas as
 # variáveis numéricas apresentam valores dentro dos limites teóricos, sendo assim,
 # podemos dizer que não há outliers.

 # Vamos carregar o dataset diamonds para verificar a existência de outliers

data(diamonds)

# Vamos filtrar apenas observações cujo corte seja "Fair".

diamonds_fair <- diamonds %>%
  filter(cut == "Fair")

# Agora queremos examinar se o preço possui outliers. Inicialmente vamos plotar
# o histograma da distruição empirica dos valores. Note que há muitos valores na
# cauda direita.

hist_diamonds_price <- ggplot(diamonds_fair, aes(x = price)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Histograma do preço",
       x = "Preço",
       y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Por fim, o boxplot mostra 148 observações consideradas outlier

boxplot(diamonds_fair$price,
        ylab = "price",
        main = "Boxplot do preço dos diamantes com corte fair"
)

out <- boxplot.stats(diamonds_fair$price)$out
out
