if (!require("shiny")) install.packages("shiny")
library(shiny)

if (!require("readxl")) install.packages("readxl")
library(readxl)

if (!require("plotly")) {
  install.packages("plotly")
}


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
                  selected = "SepalWidthCm"),
      
      # Input: Select species to include in the plot
      selectInput(inputId = "species_var",
                  label = "Select species to include:",
                  choices = c("All", "Iris-setosa", "Iris-versicolor", "Iris-virginica"),
                  selected = "All"),
      
      # Input: Select color for each species
      textInput(inputId = "setosa_color",
                label = "Setosa color:",
                value = "red"),
      
      textInput(inputId = "versicolor_color",
                label = "Versicolor color:",
                value = "blue"),
      
      textInput(inputId = "virginica_color",
                label = "Virginica color:",
                value = "green"),
      
      # Input: Select point size for each species
      sliderInput(inputId = "setosa_size",
                  label = "Setosa size:",
                  min = 1,
                  max = 20,
                  value = 10),
      
      sliderInput(inputId = "versicolor_size",
                  label = "Versicolor size:",
                  min = 1,
                  max = 20,
                  value = 10),
      
      sliderInput(inputId = "virginica_size",
                  label = "Virginica size:",
                  min = 1,
                  max = 20,
                  value = 10),
      
      # Input: Select point shape for each species
      selectInput(inputId = "setosa_shape",
                  label = "Setosa shape:",
                  choices = c("square", "circle", "triangle", "cross"),
                  selected = "square"),
      
      selectInput(inputId = "versicolor_shape",
                  label = "Versicolor shape:",
                  choices = c("square", "circle", "triangle", "cross"),
                  selected = "circle"),
      
      selectInput(inputId = "virginica_shape",
                  label = "Virginica shape:",
                  choices = c("square", "circle", "triangle", "cross"),
                  selected = "triangle")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Scatter plot ----
      plotlyOutput(outputId ="scatterPlot", height = "800px", width = "1600px")
    )
  )
)
server <- function(input, output) {
  output$scatterPlot <- renderPlotly({
    x <- iris_data[[input$x_var]]
    y <- iris_data[[input$y_var]]
    species <- iris_data$Species
    
    # filter out rows with missing values in x or y variable
    xy_data <- na.omit(data.frame(x, y, species))
    
    # create a color vector for each species
    color <- ifelse(xy_data$species == "Iris-setosa", input$setosa_color,
                    ifelse(xy_data$species == "Iris-versicolor", input$versicolor_color, 
                           input$virginica_color))
    
    # filter out rows not selected by the user
    if (input$species_var != "All") {
      xy_data <- xy_data[xy_data$species == input$species_var,]
    }
    
    # create a size vector for each species
    size <- ifelse(xy_data$species == "Iris-setosa", input$setosa_size,
                   ifelse(xy_data$species == "Iris-versicolor", input$versicolor_size, 
                          input$virginica_size))
    
    # create a shape vector for each species
    shape <- ifelse(xy_data$species == "Iris-setosa", input$setosa_shape,
                    ifelse(xy_data$species == "Iris-versicolor", input$versicolor_shape, 
                           input$virginica_shape))
    shape <- ifelse(shape == "square", 1, ifelse(shape == "circle", 0, ifelse(shape == "triangle", 2, 3)))
    
    # create a scatter plot with each species in different color, size, and shape
    plot_ly(data = xy_data, x = ~x, y = ~y, type = "scatter",
            mode = "markers",
            marker = list(color = color, size = size, symbol = shape)) %>%
      layout(xaxis = list(title = input$x_var),
             yaxis = list(title = input$y_var),
             title = paste("Scatter plot of", input$x_var, "vs", input$y_var))
  })
}

shinyApp(ui, server)
