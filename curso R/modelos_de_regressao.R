library(car)
library(tidyverse)
library(tseries)
library(broom)
library(MASS)
library(ggpubr)

# carrega os dados -------------------------------------------------------

data(mtcars)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Relação entre Peso e Consumo de Combustível",
       x = "Peso do Carro (1000 lbs)",
       y = "Consumo (mpg)") +
  theme_minimal()

# Ajustar o modelo de regressão linear
modelo <- lm(mpg ~ wt, 
             data = mtcars)

# Exibir o resumo do modelo
summary(modelo)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +  # Gráfico de dispersão
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Adiciona a reta de regressão
  labs(title = "Relação entre Peso e Consumo de Combustível",
       x = "Peso do Carro (1000 lbs)",
       y = "Consumo (mpg)") +
  theme_minimal()

# Fazer previsões para novos dados

novos_dados <- data.frame(wt = c(2.5, 3.0, 4.0))
previsoes <- predict(modelo, newdata = novos_dados)

previsoes

# Análise de resíduos

residuos <- residuals(modelo)

valores_ajustados <- fitted(modelo)

ggplot(data = data.frame(residuos, valores_ajustados), aes(x = valores_ajustados, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Resíduos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Resíduos")

ggplot(data = data.frame(residuos), aes(x = residuos)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma dos Resíduos",
       x = "Resíduos",
       y = "Frequência")

# qqplot

qqnorm(residuos)
qqline(residuos, col = "red")

# Testes de Normalidade

jarque.bera.test(residuos)

shapiro.test(residuos)

# Medidas de diagnóstico

summary(modelo)$r.squared

summary(modelo)$sigma

summary(modelo)$coefficients

AIC(modelo)
BIC(modelo)

# regressão multipla 

modelo1 <- lm(mpg ~ wt + hp + cyl + qsec, 
              data = mtcars)
modelo2 <- lm(mpg ~ wt + hp + cyl + drat, 
              data = mtcars)

diagnostico_residuos <- function(modelo) {
  
  # Resíduos e ajustes
  residuos <- residuals(modelo)
  ajustes <- fitted(modelo)
  
  # Gráfico de resíduos vs ajustes
  plot_residuos <- ggplot(data.frame(ajustes, residuos), aes(x = ajustes, y = residuos)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "Resíduos vs Ajustes", x = "Ajustes", y = "Resíduos")
  
  # Histograma dos resíduos
  hist_residuos <- ggplot(data.frame(residuos), aes(x = residuos)) +
    geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
    labs(title = "Histograma dos Resíduos", x = "Resíduos", y = "Frequência")
  
  # QQ-plot dos resíduos
  qqplot_residuos <- ggqqplot(residuos, title = "QQ-plot dos Resíduos", color = "blue")
  
  # Teste de normalidade dos resíduos - Shapiro-Wilk
  shapiro_test <- shapiro.test(residuos)
  
  # Teste de normalidade dos resíduos - Jarque-Bera
  jarque_bera_test <- jarque.bera.test(residuos)
  
  # Retornar os gráficos e os testes como uma lista
  return(list(
    plot_residuos = plot_residuos,
    hist_residuos = hist_residuos,
    qqplot_residuos = qqplot_residuos,
    shapiro_test = shapiro_test,
    jarque_bera_test = jarque_bera_test
  ))
}

diagnosticos_modelo_1 <- diagnostico_residuos(modelo1)

# Exibir os gráficos e os resultados dos testes
print(diagnosticos$plot_residuos)
print(diagnosticos$hist_residuos)
print(diagnosticos$qqplot_residuos)
print(diagnosticos$shapiro_test)
print(diagnosticos$jarque_bera_test)

diagnosticos_modelo_2 <- diagnostico_residuos(modelo2)

metricas_diagnostico <- c(AIC(modelo1), BIC(modelo1), 
                          AIC(modelo2), BIC(modelo2))
names(metricas_diagnostico) <- c("AIC model1", "BIC model1", "AIC model2", "BIC model2")

metricas_diagnostico

