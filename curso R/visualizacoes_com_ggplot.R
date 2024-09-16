library(ggplot2)
library(gapminder)
library(dplyr)

# Filtrando o dataset para o ano de 2007
dados_2007 <- gapminder %>%
  filter(year == "2007")

# Criando um gráfico de dispersão com ggplot2
ggplot(data = dados_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(title = "Relação entre PIB per capita e Expectativa de Vida (2007)",
       x = "PIB per capita",
       y = "Expectativa de Vida (anos)") +
  theme_minimal()

# Gráfico equivalente usando o R base

plot(dados_2007$gdpPercap, dados_2007$lifeExp,
     main = "Relação entre PIB per capita e Expectativa de Vida (2007)",
     xlab = "PIB per capita",
     ylab = "Expectativa de Vida (anos)",
     pch = 19,        # Tipo de ponto preenchido
     col = "blue",    # Cor dos pontos
     cex = 1.2)  

# Adicionando linha de tendência

ggplot(data = dados_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "loess", col = "blue", se = TRUE) +
  labs(title = "Relação entre PIB per capita e Expectativa de Vida (2007) com Linha de Tendência",
       x = "PIB per capita",
       y = "Expectativa de Vida (anos)") +
  theme_minimal()

# Trabalhando com Cores e Tamanhos Condicionais

ggplot(data = dados_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(title = "PIB per capita e Expectativa de Vida (2007) por Continente",
       x = "PIB per capita (log scale)",
       y = "Expectativa de Vida (anos)",
       color = "Continente",
       size = "População") +
  theme_minimal()

# Ajustando Facetas 

ggplot(data = dados_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  facet_wrap(~ continent) +
  labs(title = "PIB per capita e Expectativa de Vida por Continente (2007)",
       x = "PIB per capita",
       y = "Expectativa de Vida (anos)") +
  theme_minimal()

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  facet_grid(continent ~ year) +
  labs(title = "Relação entre PIB per capita e Expectativa de Vida",
       x = "PIB per capita",
       y = "Expectativa de Vida (anos)") +
  theme_minimal()

# Personalizando títulos e rótulos

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(
    title = "Relação entre PIB per capita e Expectativa de Vida",
    subtitle = "Dados do gapminder para o ano de 2007",
    caption = "Fonte: Base de Dados Gapminder",
    x = "PIB per capita",
    y = "Expectativa de Vida (anos)"
  ) +
  theme_gray()

# Personalizando temas

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(
    title = "Relação entre PIB per capita e Expectativa de Vida",
    x = "PIB per capita",
    y = "Expectativa de Vida (anos)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(color = "blue", size = 12),
    axis.title.y = element_text(color = "blue", size = 12),
    panel.background = element_rect(fill = "lightgrey"),
    panel.grid.major = element_line(color = "white")
  )

# Customizando escalas

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_color_brewer(palette = "BrBG") +
  labs(
    title = "Expectativa de Vida por Continente (2007)",
    x = "PIB per capita",
    y = "Expectativa de Vida (anos)",
    color = "Continente"
  ) +
  theme_minimal()

# Graficos de linha

dados_br <- gapminder %>%
  filter(country == "Brazil")

# Criar gráfico de linhas
ggplot(data = dados_br, aes(x = year, y = gdpPercap)) +
  geom_line() +
  labs(title = "Evolução do PIB per capita dos Brasil",
       x = "Ano",
       y = "PIB per capita") +
  theme_minimal()

# Barplot

ggplot(data = gapminder, aes(x = continent)) +
  geom_bar() +
  labs(title = "Número de Países por Continente",
       x = "Continente",
       y = "Número de Países")

# Histograma

ggplot(data = gapminder, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 5000) +
  labs(title = "Distribuição do PIB per capita",
       x = "PIB per capita",
       y = "Número de Países")

# Boxplot

ggplot(data = gapminder, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(title = "Expectativa de Vida por Continente",
       x = "Continente",
       y = "Expectativa de Vida (anos)")

# Gráfico de Violino

ggplot(data = gapminder, aes(x = continent, y = lifeExp)) +
  geom_violin() +
  labs(title = "Distribuição da Expectativa de Vida por Continente",
       x = "Continente",
       y = "Expectativa de Vida (anos)")