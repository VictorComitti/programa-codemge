# Semana 1 ---------------------------------------------------------------------

Dia <- c("segunda_feira", "terça-feira", "quarta-feira", 
         "quinta-feira", "sexta-feira")

vale3 <- c(891, -56, -85, -260, 544)
petr3 <- c(234, 112, -150, 389, -15)

ganhos_semanais <- data.frame(Dia, vale3, petr3)

ganhos_semanais$lucro <- ifelse(ganhos_semanais$vale3 + ganhos_semanais$petr3 > 0,
                                "sim", 
                                "não")

# semana 2

vale_3_semana_2 <- c(920, -45, -95, -270, 550)
petr_3_semana_2 <- c(310, 130, -140, 400, -10)

ganhos_semana_2 <- data.frame(Dia, vale_3_semana_2, petr_3_semana_2)

ganhos_semana_2$lucro <- ifelse(ganhos_semana_2$vale_3_semana_2 + 
                                  ganhos_semana_2$petr_3_semana_2 > 0,
                                "sim", 
                                "não")

lista_ganhos <- list(ganhos_semanais, ganhos_semana_2)
