rm(list=ls())

library(caret)
library(MASS)
library(ranger)
library(xgboost)
library(glmnet)

# Carregar o dataset Boston
data("Boston")

# Dividir o dataset em treinamento e teste
set.seed(123)  # Para reprodutibilidade
index <- createDataPartition(Boston$medv, p = 0.7, list = FALSE)
train_data <- Boston[index, ]
test_data <- Boston[-index, ]

# Definir grids de parâmetros para tuning
grid_ridge <- expand.grid(alpha = 0, lambda = 10^seq(-3, 3, length = 10))
grid_tree <- expand.grid(cp = seq(0.01, 0.1, length = 10))
grid_rf <- expand.grid(mtry = seq(2, 10, by = 2))
grid_ranger <- expand.grid(
  mtry = seq(2, 10, by = 3),
  splitrule = c("variance", "extratrees"),
  min.node.size = c(5, 10)
)
grid_xgboost <- expand.grid(
  nrounds = c(50, 150),
  eta = c(0.01, 0.1),
  max_depth = c(3, 9),
  gamma = c(0, 1, 5),
  colsample_bytree = c(0.5,  1),
  min_child_weight = c(1, 10),
  subsample = 1
)

trControl <- trainControl(
  method = "cv", 
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Definir os modelos a serem usados
models <- list(
  linear = train(medv ~ ., data = train_data, method = "lm", trControl = trControl),
  ridge = train(medv ~ ., data = train_data, method = "glmnet", tuneGrid = grid_ridge, trControl = trControl),
  tree = train(medv ~ ., data = train_data, method = "rpart", tuneGrid = grid_tree, trControl = trControl),
  rf = train(medv ~ ., data = train_data, method = "rf", tuneGrid = grid_rf, trControl = trControl),
  ranger = train(medv ~ ., data = train_data, method = "ranger", tuneGrid = grid_ranger, trControl = trControl),
  xgboost = train(medv ~ ., data = train_data, method = "xgbTree", tuneGrid = grid_xgboost, trControl = trControl)
)

# Função para calcular o R² e RMSE
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data$medv
  r_squared <- cor(predictions, actuals)^2
  rmse <- sqrt(mean((predictions - actuals)^2))
  return(c(R2 = r_squared, RMSE = rmse))
}

# Avaliar todos os modelos
model_evaluation <- sapply(models, evaluate_model, test_data = test_data)
model_evaluation <- t(model_evaluation)

# Exibir os resultados
print(model_evaluation)
