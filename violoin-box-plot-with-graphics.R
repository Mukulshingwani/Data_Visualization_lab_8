library(shiny)
library(plotly)
library(ggplot2)
library(tidyr)

data(iris)

ui <- fluidPage(
  titlePanel("Violin plot of Iris Dataset"),
  h2("I'm Mukul Shingwani"),
  h3("B20AI023"),
  div(class = "title", "Iris Dataset: Sepal Length by Species"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", label = "Select Plot Type:", 
                  choices = c("Box Plot", "Violin Plot"), 
                  selected = "Violin Plot")
    ),
    mainPanel(
      plotlyOutput("iris_plot")
    )
  )
)

server <- function(input, output) {
  plot_data <- reactive({
    if (input$plot_type == "Box Plot") {
      iris %>% 
        pivot_longer(-Species, names_to = "variable", values_to = "value") %>% 
        ggplot(aes(x = Species, y = value, fill = Species)) +
        geom_boxplot() +
        theme_classic()
    } else {
      iris %>% 
        ggplot(aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_violin() +
        geom_boxplot(width = 0.1, fill = "white", alpha = 0) +
        theme_classic()
    }
  })
  
  output$iris_plot <- renderPlotly({
    plot_data() %>% 
      ggplotly(tooltip = c("Species", "value")) %>% 
      layout(
        dragmode = "zoom", 
        xaxis = list(title = "Species"), 
        yaxis = list(title = "Sepal Length"),
        margin = list(l = 50, r = 50, b = 50, t = 50), 
        autosize = TRUE,
        height = 600,
        width = 800,
        hovermode = "closest", 
        dragmode = "lasso",
        hoverdistance = -1,
        spikedistance = -1,
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12))
      )
  })
}

shinyApp(ui, server)
