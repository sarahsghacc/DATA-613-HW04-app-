library(shiny)
library(shinythemes)
library(fresh)
library(ggplot2)

create_theme(
  theme = "default",
  bs_vars_color(
    gray_base = "#354e5c"
  ),
  bs_vars_wells(
    bg = "#90ee90",
    border = "#552D42"
  ),
  bs_vars_global(
    body_bg = "#e5ffe5"
  ),
  bs_vars_input(
    color = "#5d3954",
    border_radius = "20px"
  ),
  #
  output_file = "www/mytheme.css"
)

ui <- fluidPage(
  theme = "mytheme.css",
  titlePanel("Old Faithful Geyser Data"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        "This is a well Panel",
        textInput("plot_title", "Plot Title?", value = "Your Title"),
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30
        )
      )
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(faithful, aes(x = waiting)) +
      geom_histogram(bins = input$bins, fill = "palegreen4") +
      theme(plot.background = element_rect(fill = "palegreen1")) +
      ggtitle(input$plot_title) +
      theme(
        panel.background = element_rect(
          fill = "palegreen1",
          color = "palegreen1",
          size = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_line(
          size = 0.5, linetype = "solid",
          color = "white"
        ),
        panel.grid.minor = element_line(
          size = 0.25, linetype = "solid",
          color = "white"
        )
      )
  })
}

shinyApp(ui = ui, server = server)