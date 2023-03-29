if (!require("shiny")) install.packages("shiny")
library(shiny)

if (!require("readxl")) install.packages("readxl")
library(readxl)

iris_data <- read_excel("~/Desktop/iris_data.xlsx")

# Define UI for app that draws a scatter plot ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello World!"),
  h2("I'm Mukul Shingwani"),
  h3("B20AI023"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select variables for x and y axis
      selectInput(inputId = "x_var",
                  label = "X-axis variable:",
                  choices = c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm"),
                  selected = "SepalLengthCm"),
      
      selectInput(inputId = "y_var",
                  label = "Y-axis variable:",
                  choices = c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm"),
                  selected = "SepalWidthCm")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Scatter plot ----
      plotOutput(outputId = "scatterPlot")
      
    )
  )
)

server <- function(input, output) {
  
  # Scatter plot of the iris dataset with different colors for each species----
  # This expression that generates a scatter plot is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$x_var, input$y_var) change
  # 2. Its output type is a plot
  output$scatterPlot <- renderPlot({
    
    x    <- iris_data[[input$x_var]]
    y    <- iris_data[[input$y_var]]
    species <- iris_data$Species
    
    # create a color vector for each species
    color <- ifelse(species == "Iris-setosa", "red", ifelse(species == "Iris-versicolor", "blue", "green"))
    
    # plot scatter plot with each species in different color
    plot(x, y, pch = 16, col = color,
         xlab = input$x_var,
         ylab = input$y_var,
         main = paste("Scatter plot of", input$x_var, "vs", input$y_var))
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
