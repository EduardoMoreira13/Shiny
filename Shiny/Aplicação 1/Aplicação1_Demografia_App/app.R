# Aplicação 1 - Shiny em demografia

library(shiny)
library(readxl)
library(tidyverse)

dados_TO <- read_excel("dados_TO.xlsx")

# Definição do User Interface
ui <- fluidPage(

    # Título
    titlePanel("Natalidade / Fecundidade no Tocantins"),

    # Tema
    theme = shinythemes::shinytheme('flatly'),
    
    # Painel Principal
    mainPanel(
      # Adicionar abas
      tabsetPanel(
        
        # Aba 1 - Texto
        tabPanel('Introdução',
                 
                 br(),
                 strong("Dashboard - Estudo Demográfico sobre
                        Natalidade no Tocantins para os anos 
                        2000, 2005, 2011, 2019, 2020 e 2021"),
                 br(),
                 br(),
                 p("Essa aplicação permite ao usuário visualizar
                   as informações sobre a população feminina, taxas
                   específicas de fecundidade (TEF) e taxas específicas
                   de fecundidade feminina (TEFF) por idade e ano de interesse."),
                 br(),
                 p("TEF: Número médio de filhos nascidos vivos, tidos por 
                   mulher, por faixa etária específica do período 
                   reprodutivo. Mede a intensidade de fecundidade que as 
                   mulheres estão sujeitas em cada grupo etário, dentro do 
                   período reprodutivo (dos 15 aos 49 anos de idade)."),
                 br(),
                 p("TEFF: Número médio de filhas mulheres nascidas vivas, 
                    tidos por mulher, por faixa etária específica do 
                    período reprodutivo."),               
                 br()
                 
                 ),
        
        # Aba 2 - Tabelas
        tabPanel('Tabelas', 
                 
                 br(),
                 strong(h2("Tabela 1 - Informações para cada Faixa Etária")),
                 br(),
                 
                 selectInput(inputId = 'Faixa1', 
                             label = 'Escolha um Grupo Etário:', 
                             choices = c('15 a 19', '20 a 24',
                                         '25 a 29','30 a 34',
                                         '35 a 39','40 a 44',
                                         '45 a 49')),
                
                 br(),
                 DT::DTOutput('table1'),
                 br(),
                 
                 br(),
                 strong(h2("Tabela 2 - Informações para cada Ano")),
                 br(),
                 
                 checkboxGroupInput(inputId = "Ano", 
                                    label ="Anos de Interesse", 
                                    choices = list("2000" = 2000, 
                                    "2005" = 2005,"2011" = 2011,
                                    "2019" = 2019,"2020" = 2020,
                                    "2021" = 2021)),
                 
                 br(),
                 DT::DTOutput('table2')),
        
        # Aba 3 - Gráfico 1
        tabPanel('Gráfico 1', 
                 
                 br(),
                 strong(h3("Gráfico 1 - População Feminina")),
                 br(),
                 
                 checkboxGroupInput(inputId = 'Faixa2', 
                                    label ='Escolha um Grupo Etário:', 
                                    choices = list('15 a 19' = '15 a 19', 
                                                   '20 a 24' = '20 a 24',
                                                   '30 a 34' = '30 a 34',
                                                   '35 a 39' = '35 a 39',
                                                   '40 a 44' = '40 a 44',
                                                   '45 a 49' = '45 a 49')),
                 
                 br(),
                 plotOutput('Pop')),
        
        # Aba 4 - Gráfico 2
        tabPanel('Gráfico 2', 
                 
                 br(),
                 strong(h3("Gráfico 2 - Taxas Específicas de Fecundidade")),
                 br(),
                 
                 checkboxGroupInput(inputId = 'Faixa3', 
                                    label ='Escolha um Grupo Etário:', 
                                    choices = list('15 a 19' = '15 a 19', 
                                                   '20 a 24' = '20 a 24',
                                                   '30 a 34' = '30 a 34',
                                                   '35 a 39' = '35 a 39',
                                                   '40 a 44' = '40 a 44',
                                                   '45 a 49' = '45 a 49')),
                 
                 br(),
                 plotOutput('TEF'),
                 br(),
        
                 br(),
                 strong(h3("TFT - Taxa de Fecundidade Total")),
                 br(),
                 
                selectInput(inputId = 'Ano_Final', 
                            label = 'Escolha um Ano:', 
                            choices = c(2000, 2005,2011,2019,
                                        2020,2021)),
                 
                 br(),
                 textOutput("sel_Ano"),
                 br(),
                
                 "Uma TFT inferior a 2.1 significa",
                 span("abaixo do nível de reposição. ", 
                      style = "color:red"), 
                "Nesse sentido, a população do Tocantins tende
                 a diminuir, no longo prazo."
                
                )
      )))

# Desenvolvimento de cálculos e processamento no Server
server <- function(input, output) {
  
  # Gerar uma tabela
  output$table1 <- DT::renderDataTable(
    {dados_TO %>% 
        group_by(Ano) %>%
        filter(Idade == input$Faixa1)})
  
  # Gerar uma tabela
  output$table2 <- DT::renderDataTable(
    {dados_TO %>% 
        group_by(Idade) %>%
        filter(Ano %in% input$Ano)})
  
  # Gerar um gráfico
  output$Pop <- renderPlot({
    dados_TO %>% 
      filter(Idade %in% input$Faixa2) %>% 
      ggplot(aes(x = Ano, y = Pop_Fem, col = Idade)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "População das mulheres em Idade Reprodutiva do Tocantins segundo Grupo Etário - 2000/2021",
           x = "Anos",
           y = "População Feminina") +
      scale_y_continuous(breaks = seq(from = 20000, to = 80000, 
                                      by = 10000),
                         limits = c(20000,80000)) +
      scale_x_continuous(breaks = seq(from = 2000, to = 2022, 
                                      by = 2),
                         limits = c(2000,2022)) +
      theme_bw()
    
  })
  
  # Gerar um gráfico
  output$TEF <- renderPlot({
    dados_TO %>% 
      filter(Idade %in% input$Faixa3) %>% 
      ggplot(aes(x = Ano, y = TEF, col = Idade)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "Taxa Específica de Fecundidade - TO - 2000/2021",
           x = "Anos",
           y = "TEF por 1000 mulheres") +
      scale_y_continuous(breaks = seq(from = 0, to = 200, by = 20),
                         limits = c(0,200)) +
      scale_x_continuous(breaks = seq(from = 2000, to = 2022, by = 2),
                         limits = c(2000,2022)) +
      theme_bw()
  })
  
  # Gerar um texto interativo
  output$sel_Ano <- renderText({
    
    TEF <- dados_TO %>% 
           filter(Ano == input$Ano_Final)
    
    TFT = 5 * sum(TEF$TEF/1000)
    
    str_c("A Taxa de Fecundidade Total para o ano de ", input$Ano_Final, 
          " é aproximadamente ", TFT)
  })
  
  }

# Executar a apresentação
shinyApp(ui = ui, server = server)
