# Importação das bibliotecas
library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(caret)
library(openxlsx)

# Leitura do arquivo
data <- read.xlsx("all_leagues.xlsx", sheet = "All Leagues")

# Interface do usuário (UI)
ui <- fluidPage(
  navbarPage("Futebol Analytics",
             tabPanel("DataSet",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("liga_selector"),
                          uiOutput("team_selector"),
                          actionButton("update_filter", "Filtrar")
                        ),
                        mainPanel(
                          tableOutput("data"),
                          actionButton("full_df", "ver mais")
                        )
                      )
             ),
             tabPanel("Dashboards",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("plot_type", "Selecione o tipo de Dashboard:",
                                      choices = c("Desempenho Geral", "Comparativo de Ligas", "Estatísticas de Time")),
                          uiOutput("liga_selector_dashboard"),
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
                          uiOutput("team_selector_pred"),
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

# Servidor (Server)
server <- function(input, output, session) {
  # atualiza os seletores de ligas dinamicamente
  output$liga_selector <- renderUI({
    selectInput("liga", "Selecione a Liga:", choices = c("Todos", unique(data$league_name)))
  })
  
  # seletor de times 
  output$team_selector <- renderUI({
    req(input$liga)
    if (input$liga == "Todos") { # caso o input das ligas seja todos ele retorna apenas a opção de todos
      return(selectInput("team", "Selecione o Time:", choices = "Todos"))
    }
    selectInput("team", "Selecione o Time:", choices = c("Todos", unique(data$team[data$league_name == input$liga])))
  })
  
  # seletor de liga para o dashboard
  output$liga_selector_dashboard <- renderUI({
    req(input$plot_type)
    if(input$plot_type == "Comparativo de Ligas") {
      return(selectInput("liga_dashboard", "Selecione a Liga:", choices = ""))
    }
    selectInput("liga_dashboard", "Selecione a Liga:", choices = unique(data$league_name))
  })
  
  # seletor de times para o dashboard
  output$team_selector_pred <- renderUI({
    req(input$liga_pred)
    selectInput("team_pred", "Selecione o Time:", choices = unique(data$team[data$league_name == input$liga_pred]))
  })
  
  # exibe o DataFrame utilizado com filtro de pesquisa
  observeEvent(input$update_filter, {
    filtred_data <- data
    if (input$liga != "Todos") {
      filtred_data <- filtred_data %>% filter(league_name == input$liga)
    }
    if (input$team != "Todos") {
      filtred_data <- filtred_data %>% filter(team == input$team)
    }
    output$data <- renderTable({
      head(filtred_data)
    })
  })
  
  # mostra DataFrame completo ao clicar no botão "ver mais"
  observeEvent(input$full_df, {
    filtred_data <- data
    if (input$liga != "Todos") {
      filtred_data <- filtred_data %>% filter(league_name == input$liga)
    }
    if (input$team != "Todos") {
      filtred_data <- filtred_data %>% filter(team == input$team)
    }
    output$data <- renderTable({
      filtred_data
    })
  })
  
  # Renderiza o gráfico selecionado
  output$dashboard_plot <- renderPlot({
    req(input$update_plot)
    isolate({
      liga_data <- data %>% filter(league_name == input$liga_dashboard)
      
      # Sequência de if/else para determinar qual gráfico exibir
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
          geom_boxplot() +
          geom_jitter(width = 0.2, alpha = 0.5) +
          scale_y_continuous(labels = scales::scientific) +
          labs(title = "Comparativo de Ligas",
               x = "Liga",
               y = "Lucro da Temporada (em milhões/bilhões)",
               fill = "Liga",
               caption = "Fonte: Dados extraídos de 2023") +
          theme_minimal() +
          theme(legend.position = "none")
      } else if (input$plot_type == "Estatísticas de Time") {
        p <- ggplot(liga_data, aes(x = league_n, y = season_profit, color = as.factor(league_cup))) +
          geom_point(aes(size = as.factor(league_cont))) +
          geom_text(aes(label = team), vjust = -1, hjust = 0.5) +
          scale_y_continuous(labels = scales::scientific) +
          labs(title = "Desempenho do Time",
               x = "Posição Final no Campeonato",
               y = "Lucro da Temporada (em milhões/bilhões)",
               color = "Ganhou Campeonato Nacional",
               size = "Ganhou Campeonato Continental",
               caption = "Fonte: Dados extraídos de 2023") +
          theme_minimal()
      }
      
      p
    })
  })
  
  # Mostra insights dos gráficos
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
      
      HTML(paste("<p>", p, "</p>"))
    })
  })
  
  # Predição de rendimento
  output$prediction_result <- renderPrint({
    req(input$predict)
    isolate({
      team_data <- data %>% filter(league_name == input$liga_pred, team == input$team_pred)
      
      # Verifica se há informações suficientes para predição
      if (nrow(team_data) > 0) {
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
        
        paste("Predição de Rendimento para", input$team_pred, "no ano", input$ano_pred, ":", round(pred, 2))
      } else {
        "Não há dados suficientes para realizar a predição."
      }
    })
  })
  
  # exibe o DataFrame ao abrir a página
  output$data <- renderTable({
    head(data)
  })
}

# Executa a aplicação
shinyApp(ui = ui, server = server)