numeros <- c(1, 2, 3, 4, 5)
textos <- c("a", "b", "c")


matriz <- matrix(1:9, byrow = TRUE, nrow = 3)

colnames(matriz) <- c("col1", "col2", "col3")
rownames(matriz) <- c("linha1", "linha2", "linha3")

matriz[1,]

ganhos_semanais <- matrix(c(891, 234, -56, 112, -85, -150, -260, 389, 544, -15), 
                          byrow = TRUE,
                          nrow = 5)

colnames(ganhos_semanais) <- c("vale3", "petr3")
rownames(ganhos_semanais) <- c("segunda-feira", "terça-feira", "quarta-feira",
                               "quinta-feira", "sexta-feira")

ganhos_semanais <- cbind(ganhos_semanais, rowSums(ganhos_semanais))
ganhos_semanais <- rbind(ganhos_semanais, colSums(ganhos_semanais))

