library(car)
library(dplyr)
library(skimr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(mice)
library(missForest)
# Vamos usar o conjunto de dados Chile novamente -------------------------------
data(Chile)

# Vimos que as variáveis 'age', 'income', 'statusquo', 'vote' e 'education' possuem
# valores faltantes. Vamos examinar primeiros as variáveis numéricas.


Chile_imputed_income_media <- impute(Chile$income, mean)

Chile_imputed_income_mediana <- impute(Chile$income, median)

Chile_imputed_income_aleatorio <- impute(Chile$income, "random")

Chile_imputed_income_constante <- impute(Chile$income, 5000)

# Veja que as obervações imputadas aparecem com um *

# Agora podemos fazer o mesmo para as outras variáveis

Chile_imputed_age <- impute(Chile$age, mean)

# Esse método de imputação só é válido para variáveis numéricas. Não podemos tirar
# a média de variáveis que são fatores. Nesse caso precisamos de outras técnicas.

# O pacote é uma implementação do modelo Multivariate Imputation via Chained Equations.
# A função 'mice' aceita quatro métodos: PMM, logreg, polyreg e proportional odds model.
# O primeiro método é usado para variáveis numéricas, o segundo para variáveis binárias,
# o terceiro para variáveis categóricas não ordenadas e o último para variáveis categóricas
# ordenadas.

md.pattern(Chile)

imputed_Data <- mice(Chile, m = 1, maxit = 50,  seed = 500)

completeData <- complete(imputed_Data)

# missForest

imputed_data_missForest <- missForest(Chile)

imputed_data_missForest$ximp
