### Shiny - Desenvolvimento de app para web ###

library("shiny")
dir()

# LESSON 1 ----

runExample("01_hello")

# UI - User Interface: controls the layout and appearance 
# of your app

# Server: instructions that your computer needs to build 
# your app.

# shinyApp function: creates Shiny app objects from an 
# explicit UI/server pair.


# EXEMPLO 1 --

# Define UI for app that draws a histogram --
ui <- fluidPage(
  
  # App title --
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions --
  sidebarLayout(
    
    # Sidebar panel for inputs --
    sidebarPanel(
      
      # Input: Slider for the number of bins --
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs --
    mainPanel(
      
      # Output: Histogram --
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram --
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data --
  # with requested number of bins

  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

# Create Shiny app 
shinyApp(ui = ui, server = server)