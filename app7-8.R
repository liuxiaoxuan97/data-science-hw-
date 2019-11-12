
library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)
library(ggplot2)
data(gapminder)
names(gapminder)
# Define UI
ui <- fluidPage(
fluidRow(
  column(width = 4,
         sliderInput(inputId = "year",
                     label = "Choose a year",
                     min = 1960, max = 2018,
                     value = 1960, 
                     step = 1,sep = "",
                     # keep years in year format and not 1,960 format
                     ticks = FALSE,  # don't show tick marks on slider bar
                     animate = TRUE),
         plotOutput("plot1", height = 300,
                    # Equivalent to: click = clickOpts(id = "plot_click")
                    click = "plot1_click")
  )
),
fluidRow(
  column(width = 6,
         h4("Points near click"),
         verbatimTextOutput("click_info")
  )
)
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    gapminder %>% filter(year %in% input$year  & !is.na(fertility)) %>%
      ggplot(aes(fertility, life_expectancy, color = continent, size = population/10^6)) +
      geom_point(alpha = 0.5) +
      xlab("Fertility (Average number of children per woman)") +
      ylab("Life expectancy (Years)") +
      
      # Make the legend titles nicer
      scale_color_discrete(name = "Continent") +
      scale_size_continuous(name = "Population in millions") +
      
      # Change the title of the plot for each year
      # Returns a character vector containing a formatted combination 
      # of text and variable values
      ggtitle(sprintf("Life expectancy vs. fertility in %d", input$year)) +
      theme_bw()})
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(gapminder[,c('country','continent','life_expectancy','fertility')], input$plot1_click, addDist = TRUE)
  })
}

shinyApp(ui = ui, server = server)