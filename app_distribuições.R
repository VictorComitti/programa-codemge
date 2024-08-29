library(shiny)
library(ggplot2)
library(dplyr)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Simulação de Distribuições: Poisson, Normal e Binomial"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Escolha a distribuição:", 
                  choices = c("Poisson", "Normal", "Binomial")),
      
      # Parâmetros para a distribuição Poisson
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("lambda", "Lambda (média da distribuição Poisson):", value = 3, min = 0)
      ),
      
      # Parâmetros para a distribuição Normal
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("mean", "Média:", value = 0),
        numericInput("sd", "Desvio Padrão:", value = 1, min = 0)
      ),
      
      # Parâmetros para a distribuição Binomial
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        numericInput("size", "Número de tentativas (size):", value = 10, min = 1),
        numericInput("prob", "Probabilidade de sucesso (prob):", value = 0.5, min = 0, max = 1)
      ),
      
      # Tamanho da amostra
      numericInput("n", "Tamanho da amostra:", value = 100, min = 1),
      
      actionButton("simular", "Simular")
    ),
    
    mainPanel(
      plotOutput("histPlot"),
      plotOutput("densityPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# Servidor
server <- function(input, output) {
  
  observeEvent(input$simular, {
    dist_data <- reactive({
      if (input$dist == "Poisson") {
        rpois(input$n, lambda = input$lambda)
      } else if (input$dist == "Normal") {
        rnorm(input$n, mean = input$mean, sd = input$sd)
      } else if (input$dist == "Binomial") {
        rbinom(input$n, size = input$size, prob = input$prob)
      }
    })
    
    output$histPlot <- renderPlot({
      ggplot(data.frame(x = dist_data()), aes(x = x)) +
        geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black") +
        labs(title = paste("Histograma da Distribuição", input$dist),
             x = "Valores", y = "Densidade") +
        theme_minimal()
    })
    
    output$densityPlot <- renderPlot({
      if (input$dist == "Poisson") {
        x_vals <- 0:max(dist_data())
        df <- expand.grid(x = x_vals, taxa = input$lambda) %>%
          mutate(pmf = dpois(x, lambda = taxa))
        
        ggplot(df, aes(x = x, y = pmf)) +
          geom_line(linewidth = 1, color = "red") +
          geom_point(size = 3, color = "red") +
          labs(title = paste("Distribuição Teórica de Poisson com Lambda =", input$lambda),
               x = "Número de Eventos (X)", y = "Probabilidade") +
          theme_minimal()
        
      } else if (input$dist == "Normal") {
        x_vals <- seq(min(dist_data()), max(dist_data()), length.out = 100)
        df <- data.frame(x = x_vals, 
                         densidade = dnorm(x_vals, mean = input$mean, sd = input$sd))
        
        ggplot(df, aes(x = x, y = densidade)) +
          geom_line(size = 1, color = "red") +
          labs(title = paste("Densidade Teórica da Distribuição Normal com Média =", input$mean, 
                             "e Desvio Padrão =", input$sd),
               x = "Valores", y = "Densidade") +
          theme_minimal()
        
      } else if (input$dist == "Binomial") {
        x_vals <- 0:max(dist_data())
        df <- expand.grid(x = x_vals, size = input$size, prob = input$prob) %>%
          mutate(pmf = dbinom(x, size = size, prob = prob))
        
        ggplot(df, aes(x = x, y = pmf)) +
          geom_line(linewidth = 1, color = "red") +
          geom_point(size = 3, color = "red") +
          labs(title = paste("Distribuição Teórica Binomial com", input$size, "Tentativas e Probabilidade =", input$prob),
               x = "Número de Sucessos (X)", y = "Probabilidade") +
          theme_minimal()
      }
    })
    
    output$summary <- renderPrint({
      summary(dist_data())
    })
  })
}

# Executar a aplicação
shinyApp(ui = ui, server = server)




