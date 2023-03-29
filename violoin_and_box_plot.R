if (!require("shiny")) install.packages("shiny")
library(shiny)

if (!require("readxl")) install.packages("readxl")
library(readxl)

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

iris_data <- read_excel("~/Desktop/iris_data.xlsx")

# Define UI for app that draws a boxplot and violin plot ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Iris Boxplot and Violin Plot"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "y_var",
                  label = "Y-axis variable:",
                  choices = c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm"),
                  selected = "SepalLengthCm"),
      radioButtons(inputId = "plot_type",
                   label = "Select plot type:",
                   choices = c("Boxplot", "Violin plot"),
                   selected = "Boxplot")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Boxplot and violin plot ----
      plotOutput(outputId = "boxViolinPlot")
      
    )
  )
)

server <- function(input, output) {
  
  # Boxplot and violin plot of the iris dataset with separate species in a single plot ----
  output$boxViolinPlot <- renderPlot({
    
    y_var <- iris_data[[input$y_var]]
    species <- iris_data$Species
    
    # create a data frame with y_var and species columns
    df <- data.frame(y_var, species)
    
    # create boxplot and violin plot with separate species in a single plot using ggplot2
    if (input$plot_type == "Boxplot") {
      p <- ggplot(df, aes(x = species, y = y_var, fill = species)) +
        geom_boxplot(width = 0.5, outlier.shape = NA) +
        labs(title = paste("Boxplot of", input$y_var),
             x = "Species",
             y = input$y_var) +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(x = species, y = y_var, fill = species)) +
        geom_violin(trim = FALSE, scale = "width", width = 0.7) +
        labs(title = paste("Violin Plot of", input$y_var),
             x = "Species",
             y = input$y_var) +
        theme_minimal()
    }
    
    # display the plot
    print(p)
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
