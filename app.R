library(shiny)
library(bs4Dash)

ui <- bs4DashPage(
  title = "Teste do Shiny(bs4Dash)",
  header = bs4DashNavbar(
    skin = "dark",
    status = "primary",
    title = "Teste do bs4Dash"
  ),
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    title = "Menu",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Visualização Dashboards", tabName = "pagina1", icon = icon("dashboard")),
      bs4SidebarMenuItem("Tabelas e Predição", tabName = "pagina2", icon = icon("th"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "pagina1",
        fluidPage(
          titlePanel("Teste 1"),
          sidebarLayout(
            sidebarPanel(
              sliderInput("slider1", "Escolha um valor:", min = 1, max = 100, value = 50),
              textInput("text1", "Digite algo:")
            ),
            mainPanel(
              plotOutput("plot1"),
              textOutput("text_output1")
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "pagina2",
        fluidPage(
          titlePanel("Teste 2"),
          sidebarLayout(
            sidebarPanel(
              numericInput("num2", "Escolha um número:", value = 1, min = 0, max = 10),
              dateInput("date2", "Escolha uma data:")
            ),
            mainPanel(
              tableOutput("table2"),
              textOutput("text_output2")
            )
          )
        )
      )
    )
  ),
  controlbar = bs4DashControlbar(
    skin = "dark",
    status = "primary"
  ),
  footer = bs4DashFooter(
    fixed = TRUE,
    left = "Trabalho R",
    right = "Direitos Reservados"
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(1:input$slider1)
  })
  
  output$text_output1 <- renderText({
    paste("Você digitou:", input$text1)
  })
  
  output$table2 <- renderTable({
    data.frame(Número = input$num2, Data = as.character(input$date2))
  })
  
  output$text_output2 <- renderText({
    paste("Número escolhido:", input$num2, "Data escolhida:", input$date2)
  })
}

shinyApp(ui, server)
