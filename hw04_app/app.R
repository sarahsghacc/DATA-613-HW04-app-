library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "lux"),
  img(src = "https://upload.wikimedia.org/wikipedia/commons/c/c6/American_University_logo.svg"),
  titlePanel("Interactive Scatterplot of mtcars"),
  
  sidebarLayout(
    sidebarPanel(
      varSelectInput("var1", "Select X Variable", data = mtcars),
      varSelectInput("var2", "Select Y Variable", data = mtcars),
      radioButtons("color", "Choose Point Color",
                   choiceNames = c("Red", "Blue"),
                   choiceValues = c("red", "blue")),
      sliderInput("point_size", "Point Size", min = 1, max = 5, value = 2),
      sliderInput("alpha", "Point Transparency", min = 0.1, max = 1, value = 0.8, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("plot"),
      textOutput("note")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    req(input$var1, input$var2)
    
    var_x <- input$var1
    var_y <- input$var2
    
    validate(
      need(var_x != var_y, "Please select different variables for X and Y.")
    )
    
    ggplot(mtcars, aes(x = .data[[var_x]], y = .data[[var_y]])) +
      geom_point(color = input$color,
                 size = input$point_size,
                 alpha = input$alpha) +
      labs(x = var_x, y = var_y) +
      theme_light(base_size = 14) # theme_light blends better with Bootstrap themes
  })
  
  output$note <- renderText({
    if (input$var1 == input$var2) {
      "X and Y variables must be different."
    } else {
      paste("Plotting", input$var1, "vs", input$var2)
    }
  })
}

shinyApp(ui = ui, server = server)