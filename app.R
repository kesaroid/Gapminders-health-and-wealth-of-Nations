# Kesar TN
# kesar@Knights.ucf.edu

library(shiny)
library(ggplot2)
library(gapminder)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  
  fluidRow(
    column(12,
    titlePanel("Gapminder's Health and wealth of countries."),
    mainPanel(
      plotOutput(outputId = "gaplot",width = 800, height = 650, click = 'plot_click')
    )),
  
  fluidRow( 
    column(8, offset=2,
        verbatimTextOutput('click_info'),
        sliderInput(inputId = "year",
                    label = "Years:",
                    min = 1952,
                    max = 2007,
                    step = 5,
                    value = 1952,
                    width = "100%")
      )
    )
  )
)
  

server <- function(input, output) {
  
  df <- reactive({
    data <- gapminder %>% filter(year==input$year) %>% dplyr::select(-year)
  })
  
  output$gaplot <- renderPlot(
    df() %>%
      arrange(desc(pop)) %>%
      mutate(country = factor(country, country)) %>%
      ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
      geom_point(alpha=0.85) +
      scale_size(range = c(.1, 24), name="Population (M)") +
      scale_x_continuous(trans = 'log10') +
      ylab("Life Expectancy") +
      xlab("Wealth") +
      theme(legend.position = "none") +
      annotate("text", x = 0.7*max(df()$gdpPercap), y = 0.3*max(df()$lifeExp), label = input$year, size=25, alpha=0.3)
  )
  
  output$click_info <- renderPrint({
    countries <- nearPoints(data.frame(df()), input$plot_click)
    if (dim(countries)[1] == 0) paste0("No country Selected")
    else{
      countries %>% head()
    }
  })
}
shinyApp(ui = ui, server = server)