# Importação das bibliotecas
library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(caret)
library(openxlsx)
library(shinydashboard)
library(shinyWidgets)

# leitura do arquivo
data <- read.xlsx("all_leagues.xlsx", sheet = "All Leagues")
data <- data %>% mutate(goals_scored = as.numeric(goals_scored), 
                        goals_received = as.numeric(goals_received))

# interface do usuário (UI)
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Futebol Analytics"),
      tags$button(id = "toggle_btn", class = "btn btn-default", "Toggle Sidebar") # definindo funções de deslizamento
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar", # definindo o nome dos botões e colovando os icones
      menuItem("DataSet", tabName = "dataset", icon = icon("table")),
      menuItem("Dashboards", tabName = "dashboards", icon = icon("chart-bar")),
      menuItem("Predição de Rendimento", tabName = "prediction", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$script('
        $(document).ready(function() {  /* animação do deslizamento */
          $("#toggle_btn").click(function(){
            $(".main-sidebar").toggleClass("sidebar-collapse");
          });
        });
      '),
      tags$style(HTML('
        body, .content-wrapper, .right-side {
          background-color: #303030; /* cor do fundo */
          min-height: 100vh;
          height: auto;
        }
        .main-header .navbar {
          background-color: #333 !important; /* os slide bars e headers são pra mudar as cores */
        }
        .main-header .logo {
          background-color: #333 !important;
          color: #fff !important;
        }
        .main-sidebar {
          background-color: #222 !important;
        }
        .main-sidebar .sidebar-menu>li.active>a {
          border-left-color: #3c8dbc !important;
        }
        .table-responsive { /*  ajuste da tela */
          overflow-x: auto;
        }
        .box {
          background-color: #080808 !important; /* fundo da caixa */
          border-color: #4a4a4a !important; /* borda da caixa */
          color: #d7d8d9 !important; /* cor do texto */
        }
        .box-header {
          color: #d7d8d9 !important; /* cor do texto do cabeçalho */
          background: #4a4a4a !important; /* fundo do cabeçalho da caixa */
        }
      '))
    ),
    class = "skin-black",  # classe de tema pro corpo
    tabItems(
      tabItem(tabName = "dataset", # dados relativas a esta pagina
              fluidRow(
                box( # dados da caixa 
                  title = "Filtros",
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  uiOutput("liga_selector"),
                  uiOutput("team_selector"),
                  actionButton("update_filter", "Filtrar")
                ),
                box( # dados da caixa 
                  title = "Dados",
                  solidHeader = TRUE,
                  status = "primary",
                  div(class = "table-responsive", tableOutput("data")),
                  actionButton("full_df", "Ver mais")
                )
              )
      ),
      tabItem(tabName = "dashboards",
              fluidRow(
                box( # dados da caixa 
                  title = "Configuração do Dashboard",
                  solidHeader = TRUE,
                  status = "primary",
                  selectInput("plot_type", "Selecione o tipo de Dashboard:",
                              choices = c("Desempenho Geral", "Comparativo de Ligas", "Estatísticas de Time", "Clusters de Desempenho")),
                  uiOutput("liga_selector_dashboard"),
                  actionButton("update_plot", "Atualizar Plot")
                ),
                box( # dados da caixa 
                  title = "Dashboard",
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("dashboard_plot"),
                  uiOutput("dashboard_insight")
                )
              )
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                box( # dados da caixa 
                  title = "Configurações de Predição",
                  solidHeader = TRUE,
                  status = "primary",
                  uiOutput("liga_selector_pred"),
                  numericInput("gols", "Gols Marcados:", value = 0, min = 0),
                  numericInput("vitorias", "Vitórias:", value = 0, min = 0),
                  numericInput("derrotas", "Derrotas:", value = 0, min = 0),
                  numericInput("empates", "Empates:", value = 0, min = 0),
                  numericInput("posicao", "Posição Final:", value = 20, min = 1, max = 20),
                  selectInput("liga_cup", "Resultado no Campeonato:", choices = c("W", "L")),
                  actionButton("predict", "Predizer Rendimento")
                ),
                box( # dados da caixa 
                  title = "Resultado da Predição",
                  solidHeader = TRUE,
                  status = "primary",
                  verbatimTextOutput("prediction_result"),
                  uiOutput("model_performance")
                )
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
  
  # Seletor de ligas para a predicao
  output$liga_selector_pred <- renderUI({
    selectInput("liga_pred", "Selecione a Liga:", choices = unique(data$league_name))
  })
  
  # exibe o df utilizado com filtro de pesquisa
  observeEvent(input$update_filter, {
    filtered_data <- data 
    # ao clicar no botão de filtro a função é acionada
    # se todos estiver selecionado o if é acionado e o filtro é acionado, caso o contrario, o df inteiro é exibido
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
  
  # mostra o df completo ao clicar no botão "Ver mais"
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
          geom_bar(stat = "identity") + # bar plot simples
          scale_y_continuous(labels = scales::scientific) + # exibe em notação cientifica
          labs(title = "Desempenho Geral por Time",
               x = "Time",
               y = "Lucro da Temporada (em milhões/bilhões)",
               caption = "Fonte: Dados extraídos de 2023") + # legenda do gráfico
          theme_minimal()
      } else if (input$plot_type == "Comparativo de Ligas") {
        p <- ggplot(data, aes(x = league_name, y = season_profit, fill = league_name)) +
          geom_boxplot() + # box plot
          geom_jitter(width = 0.2, alpha = 0.5) + # dispersão dos dados no boxplot
          scale_y_continuous(labels = scales::scientific) +
          labs(title = "Comparativo de Ligas",
               x = "Liga",
               y = "Lucro da Temporada (em milhões/bilhões)",
               fill = "Liga",
               caption = "Fonte: Dados extraídos de 2023") +
          theme_minimal() +
          theme(legend.position = "none")
      } else if (input$plot_type == "Estatísticas de Time") {
        p <- ggplot(liga_data, aes(x = league_n, y = season_profit, color = as.factor(league_cup))) + # treansdorma em fator a coluna de string
          geom_point(aes(size = as.factor(league_cont))) + # o tamanho do ponto vai ser determinado pela coluna transformada em fator
          geom_text(aes(label = team), vjust = -1, hjust = 0.5) + # nome dos times em cima do ponto
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
        # seleciona as colunas pertinentes pro algoritimo de kmeans
        cluster_data <- data %>%
          select(goals_scored, goals_received, wins, losses, draws, season_profit, league_n) %>%
          na.omit() %>%
          scale()
        
        # k-means
        set.seed(123)
        k <- kmeans(cluster_data, centers = 3)  # ajuste do num de clusters
        
        # adicionando os clusters aos dados
        data$cluster <- factor(k$cluster) # transforma em fatores os grupos de clusters
        
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
      }else if (input$plot_type == "Clusters de Desempenho") {
        insight <- "Este gráfico mostra a análise de clusters de desempenho dos times. Cada cluster agrupa times com características semelhantes em termos de gols, vitórias, derrotas, empates e lucro da temporada."
      }
      
      HTML(paste("<p>", insight, "</p>"))
    })
  })
  
  # modelo de ml regressão linear
  observeEvent(input$predict, {
    req(input$liga_pred)  # verificação reativa
    
    # seleciona apenas os dados da liga
    data_liga <- data %>% filter(league_name == input$liga_pred)
    
    # transformando os valores boleanos e string em fatores
    data_liga <- data_liga %>% mutate(league_cup = as.factor(league_cup))
    
    # dividindo as variáveis
    X <- data_liga %>% select(goals_scored, goals_received, wins, losses, draws, matches_played, league_n, league_cup)
    Y <- data_liga$season_profit
    
    
    # divisão dos dados de treino e teste
    set.seed(123) # para reprodutibilidade
    index <- createDataPartition(Y, p = 0.8, list = FALSE)
    train_X <- X[index,]
    train_Y <- Y[index]
    test_X <- X[-index,]
    test_Y <- Y[-index]
    
    # modelo de regressão
    modelo <- lm(train_Y ~ ., data = as.data.frame(train_X))
    
    # previsões
    predicao_teste <- predict(modelo, newdata = as.data.frame(test_X))
    
    # Verificação dos níveis dos fatores
    new_data <- data.frame(
      goals_scored = input$gols,
      goals_received = 0, 
      wins = input$vitorias,
      losses = input$derrotas, 
      draws = input$empates, 
      matches_played = input$derrotas + input$vitorias + input$empates, 
      league_n = input$posicao,
      league_cup = factor(input$liga_cup, levels = levels(data_liga$league_cup))
    )
    
    # Verificação se há NA nos dados de entrada para predição
    if (any(is.na(new_data))) {
      output$prediction_result <- renderText({
        "Erro: Existem valores NA nos dados de entrada. Verifique as entradas."
      })
      return()
    }
    
    # valor final da predição
    predicao_final <- predict(modelo, newdata = new_data)
    format_final <- formatC(predicao_final, format = "f", digits = 2)
    
    # Verificação se a predição resultou em NA
    if (is.na(predicao_final)) {
      output$prediction_result <- renderText({
        "Erro: A predição resultou em NA. Verifique os dados de entrada e o modelo."
      })
    } else {
      output$prediction_result <- renderText({
        paste("Previsão do lucro para o(a) ", input$liga_pred, " selecionado:", format_final)
      })
    }
    
    # avaliação do modelo
    rmse <- sqrt(mean((predicao_teste - test_Y)^2))
    R2 <- summary(modelo)$r.squared
    
    # outputs de desempenho do modelo
    output$model_performance <- renderText({
      paste("RMSE do modelo: ", round(rmse, 2), "\nR² do modelo: ", round(R2 * 100, 2), "%")
    })
  })
  
  
  # inicializa o site com o tabela renderizada
  output$data <- renderTable({
    head(data)
  })
}

# Executa a aplicação
shinyApp(ui = ui, server = server)
