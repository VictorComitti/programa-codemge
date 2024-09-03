df <- data.frame(
  Nome = c("Ana", "Bruno", "Carla"),
  Idade = c(28, 35, 24),
  Salario = c(3000, 4500, 3200)
)

str(df)

# Acessando elementos

df[,1]

df[, 'Nome']

df$Idade

df[1, ]

# Filtrando linhas com base em uma condição ------------------------------------

subset(df, Idade > 30)

# Adicionando novas colunas ----------------------------------------------------

df$estado_civil <- c("solteiro", "solteiro", "casado")

df$Idade <- c(29, 35, 24)

# Exercicio ganhos semanais ----------------------------------------------------

Dia <- c("segunda_feira", "terça-feira", "quarta-feira", 
         "quinta-feira", "sexta-feira")

vale3 <- c(891, -56, -85, -260, 544)
petr3 <- c(234, 112, -150, 389, -15)

ganhos_semanais <- data.frame(Dia, vale3, petr3)

ganhos_semanais <- data.frame(
  Dia = c("segunda_feira", "terça-feira", "quarta-feira", 
          "quinta-feira", "sexta-feira"),
  vale3 = c(891, -56, -85, -260, 544),
  petr3 = c(234, 112, -150, 389, -15)
)

ganhos_semanais$lucro <- ifelse(ganhos_semanais$vale3 + ganhos_semanais$petr3 > 0,
                                "sim", 
                                "não")

subset(ganhos_semanais, lucro == 'sim')

my_list <-  list(df, ganhos_semanais)

my_list[[3]] <- "teste"

my_list[[3]]

minha_lista_aninhada <- list( pessoa1 = list(nome = "Carlos", idade = 30), 
                              pessoa2 = list(nome = "Bruna", idade = 25) )

