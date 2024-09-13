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

# a linha abaixo gera observações NA aleatoriamente na variável Sepal.Lenght

iris$Sepal.Length[sample(1:nrow(iris), 0.15 * nrow(iris))] <- NA

