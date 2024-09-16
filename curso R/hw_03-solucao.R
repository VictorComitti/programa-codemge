library(car)
library(dplyr)
library(skimr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(mice)
library(missForest)

set.seed(09092024)

data(iris)

# Criando uma cópia dos dados originais para manter os valores verdadeiros
iris_original <- iris

# Gerando observações NA aleatoriamente na variável Sepal.Length

iris$Sepal.Length[sample(1:nrow(iris), 0.15 * nrow(iris))] <- NA
# iris <- iris %>%
#   mutate(Sepal.Length = ifelse(row_number() %in% sample(1:nrow(.),
#                                                         0.15 * nrow(.)),
#                                NA, Sepal.Length))

# Separar as observações com NA para comparação posterior
na_indices <- which(is.na(iris$Sepal.Length))

# Função para calcular o RMSE entre os valores reais e imputados

rmse <- function(true_values, imputed_values) {
  sqrt(mean((true_values - imputed_values)^2))
}

# 1. Imputação pela média usando Hmisc::impute ---------------------------------
iris_mean_imputed <- iris %>%
  mutate(Sepal.Length = impute(Sepal.Length, mean))

# Comparar os valores imputados pela média com os valores reais
rmse_mean <- rmse(iris_original$Sepal.Length[na_indices],
                  iris_mean_imputed$Sepal.Length[na_indices])

# 2. Imputação pela mediana usando Hmisc::impute -------------------------------
iris_median_imputed <- iris %>%
  mutate(Sepal.Length = impute(Sepal.Length, median))

# Comparar os valores imputados pela mediana com os valores reais
rmse_median <- rmse(iris_original$Sepal.Length[na_indices],
                    iris_median_imputed$Sepal.Length[na_indices])

# 3. Imputação usando o pacote MICE --------------------------------------------

mice_imputed <- mice(iris, method = 'pmm', m = 5, maxit = 50, seed = 500)
iris_mice_imputed <- complete(mice_imputed)

# Comparar os valores imputados pelo MICE com os valores reais
rmse_mice <- rmse(iris_original$Sepal.Length[na_indices], 
                  iris_mice_imputed$Sepal.Length[na_indices])

# 4. Imputação usando missForest -----------------------------------------------

iris_missforest_imputed <- missForest(iris)$ximp

# Comparar os valores imputados pelo missForest com os valores reais
rmse_missforest <- rmse(iris_original$Sepal.Length[na_indices], 
                        iris_missforest_imputed$Sepal.Length[na_indices])

# Resultados
rmse_results <- tibble(
  Method = c("Mean Imputation", "Median Imputation", "MICE Imputation",
             "MissForest Imputation"),
  RMSE = c(rmse_mean, rmse_median, rmse_mice, rmse_missforest)
)

print(rmse_results)

# Visualizando as imputações para comparação -----------------------------------

plots <- list(
  ggplot() +
    geom_point(aes(x = iris_original$Sepal.Length[na_indices], 
                   y = iris_mean_imputed$Sepal.Length[na_indices]), 
               color = "blue") +
    labs(title = "Mean Imputation", x = "Real", y = "Imputed"),
  
  ggplot() +
    geom_point(aes(x = iris_original$Sepal.Length[na_indices], 
                   y = iris_median_imputed$Sepal.Length[na_indices]),
               color = "green") +
    labs(title = "Median Imputation", x = "Real", y = "Imputed"),
  
  ggplot() +
    geom_point(aes(x = iris_original$Sepal.Length[na_indices], 
                   y = iris_mice_imputed$Sepal.Length[na_indices]), 
               color = "red") +
    labs(title = "MICE Imputation", x = "Real", y = "Imputed"),
  
  ggplot() +
    geom_point(aes(x = iris_original$Sepal.Length[na_indices], 
                   y = iris_missforest_imputed$Sepal.Length[na_indices]), 
               color = "purple") +
    labs(title = "MissForest Imputation", x = "Real", y = "Imputed")
)

grid.arrange(grobs = plots, ncol = 2)
