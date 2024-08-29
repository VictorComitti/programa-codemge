library(shiny)
library(ggplot2)
library(datasets)
library(e1071)  # Para calcular a assimetria
library(modeest)  # Para calcular a moda
library(WDI)  # Para dataset GDP
library(AER)

data("BondYield")
data("ArgentinaCPI")
# Lista de datasets disponíveis (selecionando colunas numéricas)
available_datasets <- list(
  "Bond Yield" = BondYield,
  "Inflação Argentina" = ArgentinaCPI,
  "faithful (Waiting Time)" = faithful$waiting,
  "iris (Sepal Length)" = iris$Sepal.Length,
  "iris (Sepal Width)" = iris$Sepal.Width,
  "iris (Petal Length)" = iris$Petal.Length,
  "iris (Petal Width)" = iris$Petal.Width,
  "mtcars (Miles per Gallon)" = mtcars$mpg,
  "mtcars (Horsepower)" = mtcars$hp,
  "mtcars (Weight)" = mtcars$wt,
  "airquality (Ozone)" = airquality$Ozone,
  "airquality (Solar Radiation)" = airquality$Solar.R,
  "airquality (Wind)" = airquality$Wind,
  "pressure (Pressure)" = pressure$pressure,
  "pressure (Temperature)" = pressure$temperature,
  "ChickWeight (Weight)" = ChickWeight$weight,
  "CO2 (Uptake)" = CO2$uptake,
  "cars (Speed)" = cars$speed,
  "cars (Distance)" = cars$dist,
  "Economics (Unemployment Rate)" = ggplot2::economics$unemploy,
  "Economics (Personal Savings Rate)" = ggplot2::economics$psavert,
  "GDP (GDP per Capita)" = WDI::WDI(country = "US", indicator = "NY.GDP.PCAP.CD")$value,
  "GDP (GDP Growth Rate)" = WDI::WDI(country = "US", indicator = "NY.GDP.MKTP.KD.ZG")$value
)

# Definição da interface do usuário
ui <- fluidPage(
  titlePanel("Análises Interativas: Histograma, Boxplot e Violin Chart"),
  tabsetPanel(
    tabPanel("Histograma",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "dataset",
                   label = "Escolha o dataset:",
                   choices = names(available_datasets)
                 ),
                 sliderInput(
                   inputId = "bins",
                   label = "Número de classes:",
                   min = 1,
                   max = 100,
                   value = 30
                 ),
                 selectInput(
                   inputId = "color",
                   label = "Cor do histograma:",
                   choices = c("blue", "red", "green", "purple", "orange"),
                   selected = "blue"
                 )
               ),
               mainPanel(
                 plotOutput(outputId = "distPlot"),
                 tableOutput(outputId = "statsTable")
               )
             )
    ),
    tabPanel("Boxplot",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "boxplot_dataset",
                   label = "Escolha o dataset:",
                   choices = names(available_datasets)
                 ),
                 selectInput(
                   inputId = "boxplot_color",
                   label = "Cor do boxplot:",
                   choices = c("blue", "red", "green", "purple", "orange"),
                   selected = "green"
                 ),
                 checkboxInput(
                   inputId = "highlight_outliers",
                   label = "Colorir outliers de vermelho",
                   value = FALSE
                 )
               ),
               mainPanel(
                 plotOutput(outputId = "boxplot")
               )
             )
    ),
    tabPanel("Violin Chart",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "violin_dataset",
                   label = "Escolha o dataset:",
                   choices = names(available_datasets)
                 ),
                 selectInput(
                   inputId = "violin_color",
                   label = "Cor do gráfico de violino:",
                   choices = c("blue", "red", "green", "purple", "orange"),
                   selected = "purple"
                 ),
                 checkboxInput(
                   inputId = "show_median",
                   label = "Incluir Mediana",
                   value = TRUE
                 ),
                 checkboxInput(
                   inputId = "show_boxplot",
                   label = "Incluir Boxplot",
                   value = TRUE
                 )
               ),
               mainPanel(
                 plotOutput(outputId = "violinPlot")
               )
             )
    )
  ),
  hr(),
  fluidRow(
    column(12, 
           div(style = "text-align: center;", 
               tags$footer("Desenvolvido para o programa de Ciência de Dados Aplicada à Finanças da Codemge",
                           style = "font-size: 12px; color: grey;")
           )
    )
  )
)

# Definição da lógica do servidor
server <- function(input, output) {
  # Histograma com linhas tracejadas para média e mediana usando ggplot2
  output$distPlot <- renderPlot({
    dataset <- available_datasets[[input$dataset]]
    
    mean_value <- mean(dataset, na.rm = TRUE)
    median_value <- median(dataset, na.rm = TRUE)
    
    ggplot(data = data.frame(dataset), aes(x = dataset)) +
      geom_histogram(
        bins = input$bins, 
        fill = input$color, 
        color = "white"
      ) +
      geom_vline(aes(xintercept = mean_value), color = "blue", linetype = "dashed", size = 1.2) +
      geom_vline(aes(xintercept = median_value), color = "red", linetype = "dashed", size = 1.2) +
      labs(
        title = paste("Histograma de", input$dataset),
        x = "Valores",
        y = "Frequência"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$statsTable <- renderTable({
    dataset <- available_datasets[[input$dataset]]
    stats <- c(
      Media = mean(dataset, na.rm = TRUE),
      Mediana = median(dataset, na.rm = TRUE),
      Desvio_Padrao = sd(dataset, na.rm = TRUE),
      Variancia = var(dataset, na.rm = TRUE),
      Q1 = quantile(dataset, 0.25, na.rm = TRUE),
      Q3 = quantile(dataset, 0.75, na.rm = TRUE),
      Skewness = skewness(dataset, na.rm = TRUE)
    )
    as.data.frame(t(stats), row.names = "Estatística")
  })
  
  # Boxplot interativo usando ggplot2
  output$boxplot <- renderPlot({
    dataset <- available_datasets[[input$boxplot_dataset]]
    
    p <- ggplot(data = data.frame(dataset), aes(x = "", y = dataset)) +
      geom_boxplot(
        fill = input$boxplot_color, 
        staplewidth = 0.3,
        color = "black", 
        outlier.colour = ifelse(input$highlight_outliers, "red", input$boxplot_color)
      ) +
      labs(
        title = paste("Boxplot de", input$boxplot_dataset),
        y = "Valores"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(p)
  })
  
  # Violin Chart interativo usando ggplot2
  output$violinPlot <- renderPlot({
    dataset <- available_datasets[[input$violin_dataset]]
    
    p <- ggplot(data = data.frame(dataset), aes(x = "", y = dataset)) +
      geom_violin(fill = input$violin_color)
    
    # Adicionar boxplot se selecionado
    if (input$show_boxplot) {
      p <- p + geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5)
    }
    
    # Adicionar linha de mediana se selecionado
    if (input$show_median) {
      median_value <- median(dataset, na.rm = TRUE)
      p <- p + geom_hline(aes(yintercept = median_value), color = "red", linetype = "dashed", size = 1.2)
    }
    
    p <- p +
      labs(
        title = paste("Violin Chart de", input$violin_dataset),
        y = "Valores"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(p)
  })
}

# Executa o aplicativo Shiny
shinyApp(ui = ui, server = server)