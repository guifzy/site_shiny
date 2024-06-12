# Importação das bibliotecas
library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(caret)
library(openxlsx)

# Leitura do arquivo
data <- read.xlsx("all_leagues.xlsx", sheet = "All Leagues")
data <- data %>% mutate(goals_scored = as.numeric(goals_scored))
data <- data %>% mutate(goals_received = as.numeric(goals_received))

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
                          actionButton("full_df", "Ver mais")
                        )
                      )
             ),
             tabPanel("Dashboards",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("plot_type", "Selecione o tipo de Dashboard:",
                                      choices = c("Desempenho Geral", "Comparativo de Ligas", "Estatísticas de Time", "Clusters de Desempenho")),
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
  # armazena o valor da liga escolhido para o filtro do df
  output$liga_selector <- renderUI({
    selectInput("liga", "Selecione a Liga:", choices = c("Todos", unique(data$league_name)))
  })
  
  # armazena o valor do time escolhido para o filtro do df
  output$team_selector <- renderUI({
    req(input$liga)
    if (input$liga == "Todos") {
      return(selectInput("team", "Selecione o Time:", choices = "Todos"))
    }
    selectInput("team", "Selecione o Time:", choices = c("Todos", unique(data$team[data$league_name == input$liga])))
  })
  
  # armazena o valor da liga para o dashboard
  output$liga_selector_dashboard <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "Comparativo de Ligas") {
      return(selectInput("liga_dashboard", "Selecione a Liga:", choices = "")) # caso escolha comparação de ligas o botão de time some
    }
    if (input$plot_type == "Clusters de Desempenho") { # caso escolha o grafico de cluster, muda as opções
      return(selectInput("relation_cluster", "Selecione a Relação:", choices = c("Gols Marcados/Recebidos", "Gols em Relação a Renda", "Relação Vitória/Derrota", "Vitórias em Relação a Renda")))
    }
    selectInput("liga_dashboard", "Selecione a Liga:", choices = unique(data$league_name)) #armazena o nome da liga em 'liga_dashboard'
  })
  
  # Seletor de times para a predicao
  output$team_selector_pred <- renderUI({
    req(input$liga_pred)
    selectInput("team_pred", "Selecione o Time:", choices = unique(data$team[data$league_name == input$liga_pred]))
  })
  
  # Exibe o DataFrame utilizado com filtro de pesquisa
  observeEvent(input$update_filter, {
    filtered_data <- data
    if (input$liga != "Todos") {
      filtered_data <- filtered_data %>% filter(league_name == input$liga)
    }
    if (input$team != "Todos") {
      filtered_data <- filtered_data %>% filter(team == input$team)
    }
    output$data <- renderTable({
      head(filtered_data)
    })
  })
  
  # Mostra DataFrame completo ao clicar no botão "Ver mais"
  observeEvent(input$full_df, {
    filtered_data <- data
    if (input$liga != "Todos") {
      filtered_data <- filtered_data %>% filter(league_name == input$liga)
    }
    if (input$team != "Todos") {
      filtered_data <- filtered_data %>% filter(team == input$team)
    }
    output$data <- renderTable({
      filtered_data
    })
  })
  
  # Renderiza o gráfico selecionado
  output$dashboard_plot <- renderPlot({
    req(input$update_plot)
    isolate({
      liga_data <- data %>% filter(league_name == input$liga_dashboard) # filtra a partir da escolha de liga
      
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
      } else if (input$plot_type == "Clusters de Desempenho") {
        # normalização dos dados
        cluster_data <- data %>%
          select(goals_scored, goals_received, wins, losses, draws, season_profit, league_n) %>%
          na.omit() %>%
          scale()
        
        # k-means
        set.seed(123)
        k <- kmeans(cluster_data, centers = 3)  # ajuste do num de clusters
        
        # adicionando os clusters aos dados
        data$cluster <- factor(k$cluster)
        
        # plot dos clusters
        
        if (input$relation_cluster == "Gols Marcados/Recebidos"){
          p <- ggplot(data, aes(x = goals_scored, y = goals_received, color = cluster)) +
            geom_point() +
            labs(title = "Desempenho em Relação a Gols",
                 x = "Gols Marcados",
                 y = "Gols Recebidos",
                 z = "Lucro da Temporada",
                 color = "Cluster",
                 caption = "Fonte: Dados extraídos de 2023") +
            theme_minimal()
        } else if(input$relation_cluster == "Gols em Relação a Renda"){
          p <- ggplot(data, aes(x = goals_scored, y = season_profit, color = cluster)) +
            geom_point() +
            labs(title = "Gols em Relação a Renda",
                 x = "Gols Marcados",
                 y = "Lucro da Temporada",
                 color = "Cluster",
                 caption = "Fonte: Dados extraídos de 2023") +
            theme_minimal()
        } else if(input$relation_cluster == "Relação Vitória/Derrota"){
          p <- ggplot(data, aes(x = wins, y = losses, color = cluster)) +
            geom_point() +
            labs(title = "Relação Vitória/Derrota",
                 x = "Vitórias",
                 y = "Derrotas",
                 color = "Cluster",
                 caption = "Fonte: Dados extraídos de 2023") +
            theme_minimal()
        } else if(input$relation_cluster == "Vitórias em Relação a Renda"){
          p <- ggplot(data, aes(x = wins, y = season_profit, color = cluster)) +
            geom_point() +
            labs(title = "Vitórias em Relação a Renda",
                 x = "Vitórias",
                 y = "Lucro da Temporada",
                 color = "Cluster",
                 caption = "Fonte: Dados extraídos de 2023") +
            theme_minimal()
        }
      }
      
      p
    })
  })
  
  # Mostra insights dos gráficos
  output$dashboard_insight <- renderUI({
    req(input$update_plot)
    isolate({
      insight <- ""
      if (input$plot_type == "Desempenho Geral") {
        insight <- "Este gráfico mostra o lucro da temporada de cada time. Os valores em milhões/bilhões são representados em notação científica. Times com maior lucro estão mais bem posicionados financeiramente."
      } else if (input$plot_type == "Comparativo de Ligas") {
        insight <- "Este gráfico compara o lucro da temporada entre diferentes ligas. Os boxplots mostram a distribuição dos lucros dentro de cada liga, com os pontos indicando os times individuais. Use este gráfico para identificar ligas mais lucrativas e a variação de lucros entre os times. Cada região tem sua respectiva moeda, os valores estão padronizados."
      } else if (input$plot_type == "Estatísticas de Time") {
        insight <- "Este gráfico mostra o desempenho dos times em termos de lucro da temporada e posição final no campeonato. Pontos coloridos indicam se o time ganhou o campeonato nacional e o tamanho dos pontos indica se o time ganhou o campeonato continental. As etiquetas mostram o nome de cada time."
      }
      
      HTML(paste("<p>", insight, "</p>"))
    })
  })
  
  output$data <- renderTable({
    head(data)
  })
}

# Executa a aplicação
shinyApp(ui = ui, server = server)