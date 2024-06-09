# import das bibliotecas
library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(caret) 
library(openxlsx)

# a ui está bem padrão ainda, vou atualizar o design depois de terminar as funcionalidades

# leitura do arquivo 
data <- read.xlsx("all_leagues.xlsx", sheet = "All Leagues")

# front
ui <- fluidPage(
  navbarPage("Futebol Analytics",
             tabPanel("Dashboards",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("plot_type", "Selecione o tipo de Dashboard:",
                                      choices = c("Desempenho Geral", "Comparativo de Ligas", "Estatísticas de Time")),
                          uiOutput("liga_selector"),
                          actionButton("update_plot", "Atualizar Plot")
                        ),
                        mainPanel(
                          plotOutput("dashboard_plot"),
                          uiOutput("dashboard_insight")
                        )
                      )
             ),
             tabPanel("Predição de Rendimento",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("liga_pred", "Selecione a Liga:", choices = unique(data$league_name)),
                          uiOutput("team_selector"),
                          numericInput("ano_pred", "Ano de Predição:", value = 2024, min = 2020, max = 2030),
                          actionButton("predict", "Predizer Rendimento")
                        ),
                        mainPanel(
                          verbatimTextOutput("prediction_result")
                        )
                      )
             )
  )
)

# back
server <- function(input, output, session) {
  # atualiza os seletores de ligas dinamicamente
  output$liga_selector <- renderUI({
    selectInput("liga", "Selecione a Liga:", choices = unique(data$league_name))
  })
  
  output$team_selector <- renderUI({
    selectInput("team", "Selecione o Time:", choices = unique(data$team[data$league_name == input$liga_pred]))
  })
  
  # renderiza o gráfico selecionado
  output$dashboard_plot <- renderPlot({
    req(input$update_plot) # request para exibir os graficos
    isolate({
      liga_data <- data %>% filter(league_name == input$liga) # selecione apenas as linhas com filtro da coluna da liga
    
      # sequencia de if/else para determinar qual grafico exibir
      if (input$plot_type == "Desempenho Geral") {
        p <- ggplot(liga_data, aes(x = cod, y = season_profit)) + 
          geom_bar(stat = "identity") + 
          scale_y_continuous(labels = scales::scientific) +
          labs(title = "Desempenho Geral por Time",
               x = "Time",
               y = "Lucro da Temporada (em milhões/bilhões)",
               caption = "Fonte: Dados extraídos de 2023") +
          theme_minimal()
      } else if (input$plot_type == "Comparativo de Ligas") {
        p <- ggplot(data, aes(x = league_name, y = season_profit, fill = league_name)) + 
          geom_boxplot() + # grafico de barra
          geom_jitter(width = 0.2, alpha = 0.5) +  
          scale_y_continuous(labels = scales::scientific) +
          labs(title = "Comparativo de Ligas",
               x = "Liga",
               y = "Lucro da Temporada (em milhões/bilhões)",
               fill = "Liga",
               caption = "Fonte: Dados extraídos de 2023") +
          theme_minimal() +
          theme(legend.position = "none")  # removi a legenda para deixar mais claro 
      } else if (input$plot_type == "Estatísticas de Time") {
        p <- ggplot(liga_data, aes(x = league_n, y = season_profit, color = as.factor(league_cup))) +
          geom_point(aes(size = as.factor(league_cont))) + # transformei os valores boleanos em fatores
                                                           # para agrupar os times em duas categorias
          geom_text(aes(label = team), vjust = -1, hjust = 0.5) + # nome dos times dentro do grafico
          scale_y_continuous(labels = scales::scientific) +
          labs(title = "Desempenho do Time",
               x = "Posição Final no Campeonato",
               y = "Lucro da Temporada (em milhões/bilhões)",
               color = "Ganhou Campeonato Nacional",
               size = "Ganhou Campeonato Continental",
               caption = "Fonte: Dados extraídos de 2023") +
          theme_minimal()
      }
      
      p # variavel com o grafico selecionado
    })
  })
  
  # mostra insights dos gráficos
  output$dashboard_insight <- renderUI({
    req(input$update_plot)
    isolate({
      if (input$plot_type == "Desempenho Geral") {
        p <- "Este gráfico mostra o lucro da temporada de cada time. 
             Os valores em milhões/bilhões são representados em notação científica. 
             Times com maior lucro estão mais bem posicionados financeiramente."
      } else if (input$plot_type == "Comparativo de Ligas") {
        p <- "Este gráfico compara o lucro da temporada entre diferentes ligas. 
             Os boxplots mostram a distribuição dos lucros dentro de cada liga, com os pontos indicando os times individuais. 
             Use este gráfico para identificar ligas mais lucrativas e a variação de lucros entre os times. 
             Cada região tem sua respectiva moeda, os valores estão padronizados."
      } else if (input$plot_type == "Estatísticas de Time") {
        p <- "Este gráfico mostra o desempenho dos times em termos de lucro da temporada e posição final no campeonato. 
             Pontos coloridos indicam se o time ganhou o campeonato nacional e o tamanho dos pontos indica se o time ganhou o campeonato continental. 
             As etiquetas mostram o nome de cada time."
      }
      
      HTML(paste("<p>", p, "</p>")) # indica o tipo de texto com o html
    })
  })
  
  # o ml dessa parte ta dando problema ainda, vou atualizar ele até terça de tarde
  # Predição de rendimento
  output$prediction_result <- renderPrint({
    req(input$predict)
    isolate({
      team_data <- data %>% filter(league_name == input$liga_pred, team == input$team)
      
      # Verifica se há informações suficientes para predição
      if(nrow(team_data) > 0) {
        model <- lm(season_profit ~ kit + financial_fp + renda_19 + renda_20 + renda_21 + renda_22 +
                      despesa_19 + despesa_20 + despesa_21 + despesa_22, data = team_data)
        pred <- predict(model, newdata = data.frame(kit = mean(team_data$kit, na.rm = TRUE),
                                                    financial_fp = team_data$financial_fp[1],
                                                    renda_19 = mean(team_data$renda_19, na.rm = TRUE),
                                                    renda_20 = mean(team_data$renda_20, na.rm = TRUE),
                                                    renda_21 = mean(team_data$renda_21, na.rm = TRUE),
                                                    renda_22 = mean(team_data$renda_22, na.rm = TRUE),
                                                    despesa_19 = mean(team_data$despesa_19, na.rm = TRUE),
                                                    despesa_20 = mean(team_data$despesa_20, na.rm = TRUE),
                                                    despesa_21 = mean(team_data$despesa_21, na.rm = TRUE),
                                                    despesa_22 = mean(team_data$despesa_22, na.rm = TRUE)))
        
        paste("Predição de Rendimento para", input$team, "no ano", input$ano_pred, ":", round(pred, 2))
      } else {
        "Não há dados suficientes para realizar a predição."
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
