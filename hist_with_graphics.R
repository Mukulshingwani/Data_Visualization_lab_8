library(ggplot2)
library(shiny)
library(plotly)

data(iris)

ui <- fluidPage(
  # Define input UI for selecting bin sizes and colors
  titlePanel("Histograms of Iris Dataset"),
  h2("I'm Mukul Shingwani"),
  h3("B20AI023"),
  sidebarLayout(
    sidebarPanel(
      lapply(
        c("sepal_length_bins", "sepal_width_bins", "petal_length_bins", "petal_width_bins"),
        function(input_id) {
          sliderInput(
            input_id,
            label = paste0(strsplit(input_id, "_")[[1]][1], " ", strsplit(input_id, "_")[[1]][2], " Bin Size"),
            min = 0.1, max = 1, value = 0.1
          )
        }
      ),
      lapply(
        c("sepal_length_color", "sepal_width_color", "petal_length_color", "petal_width_color"),
        function(input_id) {
          selectInput(
            input_id,
            label = paste0(strsplit(input_id, "_")[[1]][1], " ", strsplit(input_id, "_")[[1]][2], " Color"),
            choices = c("Red", "Blue", "Green", "Cyan"), selected = "Red"
          )
        }
      )
    ),
    mainPanel(
      lapply(
        c("sepal_length_hist", "sepal_width_hist", "petal_length_hist", "petal_width_hist"),
        function(output_id) {
          plotlyOutput(output_id, height = 300, width = "100%")
        }
      )
    )
  )
)

server <- function(input, output) {
  
  # Render the histograms using plotly
  output$sepal_length_hist <- renderPlotly({
    p <- ggplot(iris, aes(x = Sepal.Length)) +
      geom_histogram(binwidth = input$sepal_length_bins, fill = input$sepal_length_color) +
      labs(x = "Sepal Length", y = "Frequency") +
      coord_cartesian(xlim = c(4, 8)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic()
    
    ggplotly(p) %>% 
      layout(
        xaxis = list(title = "Sepal Length", range = c(4,8), showspikes = TRUE),
        yaxis = list(title = "Frequency", showspikes = TRUE),
        dragmode = "pan",
        hovermode = FALSE
      )
  })
  
  output$sepal_width_hist <- renderPlotly({
    p <- ggplot(iris, aes(x = Sepal.Width)) +
      geom_histogram(binwidth = input$sepal_width_bins, fill = input$sepal_width_color) +
      labs(x = "Sepal Width", y = "Frequency") +
      coord_cartesian(xlim = c(2, 5)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic()
    
    ggplotly(p) %>% 
      layout(
        xaxis = list(title = "Sepal Width", range = c(2,5), showspikes = TRUE),
        yaxis = list(title = "Frequency", showspikes = TRUE),
        dragmode = "pan",
        hovermode = FALSE
      )
  })
  
  output$petal_length_hist <- renderPlotly({
    p <- ggplot(iris, aes(x = Petal.Length)) +
      geom_histogram(binwidth = input$petal_length_bins, fill = input$petal_length_color) +
      labs(x = "Petal Length", y = "Frequency") +
      coord_cartesian(xlim = c(0, 8)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic()
    
    ggplotly(p) %>% 
      layout(
        xaxis = list(title = "Petal Length", range = c(0,8), showspikes = TRUE),
        yaxis = list(title = "Frequency", showspikes = TRUE),
        dragmode = "pan",
        hovermode = FALSE
      )
  })
  
  output$petal_width_hist <- renderPlotly({
    p <- ggplot(iris, aes(x = Petal.Width)) +
      geom_histogram(binwidth = input$petal_width_bins, fill = input$petal_width_color) +
      labs(x = "Petal Width", y = "Frequency") +
      coord_cartesian(xlim = c(0, 4)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic()
    
    ggplotly(p) %>% 
      layout(
        xaxis = list(title = "Petal Width", range = c(0, 4), fixedrange = FALSE),
        yaxis = list(title = "Frequency", fixedrange = FALSE),
        dragmode = "pan",
        hovermode = "x unified"
      )
  })
}

shinyApp(ui = ui, server = server)

