install.packages("shinydashboard")
library(shinydashboard)
library(shiny)
library(ggplot2)
install.packages('bootstrap3')


pacman::p_load(shiny, shinydashboard, bootstrap3, ggplot2)

#Para alterarmos as informações e cores, utilizaremos o BootStrap 3

ui <-
  
  dashboardPage( skin = 'red',
  dashboardHeader(title = 'Dashboard CE2',
                   dropdownMenu(type = 'messages',
                                messageItem( from = 'Guilherme Souza',
                                             message = 'Gol do Flamengo. 87 é nosso',
                                             icon = icon('trophy'),
                                             time = '14:37')),
                   dropdownMenu(type = 'tasks'),
                   dropdownMenu(type = 'notifications',
                                notificationItem(text = "Faça um dashboard do McDonald's",
                                                 icon = icon('warning')))),
  dashboardSidebar(
    sidebarMenu(sliderInput("Ano", 'Ano desejado', min = 2000, max = 2022, value = 2010),
      menuItem('Link do GitHub',
               icon = icon('file-code-o'),
               href = 'https://github.com/'),
      menuItem('Gráficos',
               tabName = 'Gráficos',
               icon = icon('dashboard')),
      menuItem('Finanças',
               icon = icon('money-bill-1'))
    )
  ),
  dashboardBody(fluidRow(box(title = 'Sumário', background = 'red', width = 4, textOutput('texto')),
                         box(title = 'Histograma das Categorias',
                             width = 10,
                             status = 'info',
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             plotOutput('graph', width = '1300')),
                         box(title = 'Frequência das Calorias',
                             width = 10,
                             status = 'info',
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             plotOutput('graph2', width = '1300')))
))
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$texto <- renderText({
    paste0("Dashboard explicitando dados acerca do cardápio do McDonald's")
  })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    output$graph <- renderPlot({
      ggplot(categorias, aes(x = Category, y = n)) +
        geom_bar(stat = "identity", fill = '#A11D21', width = 0.7) +
        labs( x = "Categorias", y = "Frequências")+
        theme_bw()
    })
    output$graph2 <- renderPlot({
      
      ggplot(calorias, aes(x = Calories, y = n)) +
        geom_point(stat = "identity", color = '#A11D21', size = 1.5) +
        labs( x = "Calorias", y = "Frequências")+
        theme_bw()
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
