library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "lux"),
  img(src = "https://upload.wikimedia.org/wikipedia/commons/c/c6/American_University_logo.svg", height = "60px"),
  titlePanel("MTCARS Interactive Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      varSelectInput("xvar", "Select X Variable", data = mtcars),
      varSelectInput("yvar", "Select Y Variable", data = mtcars),
      varSelectInput("catvar", "Select Discrete (Factor) Variable", data = mtcars),
      varSelectInput("contvar", "Select Continuous Variable", data = mtcars),
      radioButtons("color", "Choose Point Color", c("Red" = "red", "Blue" = "blue")),
      sliderInput("point_size", "Point Size", 1, 5, value = 2),
      sliderInput("alpha", "Transparency", 0.1, 1, value = 0.8, step = 0.1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("datatable")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Scatterplot", plotOutput("scatterplot"), textOutput("scatter_note")),
        tabPanel("BoxPlot", plotOutput("boxplot")),
        tabPanel("Bar", plotOutput("barplot")),
        tabPanel("Histogram", plotOutput("histplot"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Data Tab
  output$datatable <- renderTable({
    mtcars
  })
  
  # Summary Tab
  output$summary <- renderPrint({
    summary(mtcars)
  })
  
  # Scatterplot Tab
  output$scatterplot <- renderPlot({
    req(input$xvar, input$yvar)
    validate(need(input$xvar != input$yvar, "Choose different variables for X and Y"))
    ggplot(mtcars, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point(color = input$color, size = input$point_size, alpha = input$alpha) +
      labs(x = input$xvar, y = input$yvar)
  })
  
  output$scatter_note <- renderText({
    if (input$xvar == input$yvar) {
      "X and Y must be different"
    } else {
      paste("Showing", input$yvar, "vs", input$xvar)
    }
  })
  
  # BoxPlot Tab
  output$boxplot <- renderPlot({
    ggplot(mtcars, aes(x = factor(1), y = .data[[input$contvar]])) +
      geom_boxplot(fill = "skyblue") +
      labs(x = "", y = input$contvar)
  })
  
  # Bar Plot Tab
  output$barplot <- renderPlot({
    ggplot(mtcars, aes(x = factor(.data[[input$catvar]]))) +
      geom_bar(fill = "steelblue") +
      labs(x = input$catvar, y = "Count")
  })
  
  # Histogram Tab
  output$histplot <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$contvar]])) +
      geom_histogram(fill = "orange", bins = 10, color = "white") +
      labs(x = input$contvar, y = "Frequency")
  })
}

shinyApp(ui, server)
