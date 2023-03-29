#~/Desktop/iris_data.xlsx
if (!require("shiny")) install.packages("shiny")
library(shiny)

if (!require("datasets")) install.packages("datasets")
library(datasets)

# Define UI for app that draws histograms ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Histograms of Iris Dataset"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histograms ----
      lapply(colnames(iris), function(col) {
        plotOutput(outputId = paste0(col, "Plot"))
      })
      
    )
  )
)

server <- function(input, output) {
  
  # Histograms of the iris dataset for all columns ----
  # with requested number of bins
  # These expressions that generate histograms are wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. They are "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Their output types are plots
  lapply(colnames(iris), function(col) {
    output[[paste0(col, "Plot")]] <- renderPlot({
      tapply(as.numeric(iris_data$PetalWidthCm), iris_data$Species, min)
      x    <- iris[, col]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x, breaks = bins, col = "blue", border = "orange",
           xlab = col,
           main = paste0("Histogram of ", col))
      
    })
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)