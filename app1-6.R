# Homework #3 Shiny app
# Due November 1, 2019

library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)
library(ggplot2)
data(gapminder)
names(gapminder)
View(gapminder)
us<-gapminder%>%filter(country=='United States')%>%summarise(min(year),max(year))
us
gg <- gapminder[, c("country", "infant_mortality","life_expectancy", "gdp", "continent","region")]
gg2<-gapminder[,c('life_expectancy', 'fertility', 'country', 'continent')] 
ui = fluidPage(
  # Change theme to darkly
  theme = shinythemes::shinytheme("darkly"),
  # Application title
  titlePanel("My More Advanced Shiny App"),
  
  # Create an app with 2 tabs
  tabsetPanel(
    # First tab: 
    # Scatter plot that compares life expectancy and fertility
    # for each country over time with an animated bar for the years
    
    # Second tab: tile plot of infectious diseases
    # Include dropdown menu of diseases to choose from 
    tabPanel("life_exp vs year",
             sidebarLayout(
               sidebarPanel(
                 # Paragraph with some text about vaccines and the data source
                 # strong("Vaccines") makes the word Vaccines bold
                 p("line plots of  year and life expectancy for us and other countries."),
                 
                 # Adding br() will add space between the text above and the dropdown
                 # menu below
                 br(),
                 
                 # Dropdown menu that allows the user to choose a disease
                 selectInput("country", label = "Select country",
                             choices = as.list(levels(gapminder$country)))
               ),
               mainPanel(
                 plotOutput("linePlot")
               )
             )),
    tabPanel("life_exp vs fertility",
             fluidRow(
               column(width = 4,selectInput('year','year',gapminder$year))
             ),
             fluidRow(column(width=8, plotOutput("scatterPlot", height = 300,
                                                 # Equivalent to: click = clickOpts(id = "plot_click")
                                                 click = "plot_click",
                                                 brush = brushOpts(
                                                   id = "plot_brush"
                                                 )))
             ),
             fluidRow(
               column(width = 6,
                      h4("Points near click"),
                      verbatimTextOutput("click1_info")
               ),
               column(width = 6,
                      h4("Brushed points"),
                      verbatimTextOutput("brush1_info")
               )
             )
    ),
  tabPanel("life_exp vs inf_mortality",
           fluidRow(
             column(width = 4,selectInput('year1','year1',gapminder$year))
           ),
           fluidRow(column(width=8, plotOutput("plot1", height = 300,
                                               # Equivalent to: click = clickOpts(id = "plot_click")
                                               click = "plot1_click",
                                               brush = brushOpts(
                                                 id = "plot1_brush"
                                               )))
           ),
           fluidRow(
             column(width = 6,
                    h4("Points near click"),
                    verbatimTextOutput("click_info")
             ),
             column(width = 6,
                    h4("Brushed points"),
                    verbatimTextOutput("brush_info")
             )
           )
  )
  )
  
  )


server = function(input, output) {
  
  # Scatterplot of fertility rate vs life expectancy
  output$linePlot <- renderPlot({
    gapminder %>% filter(country %in% c(input$country,'United States')) %>%
      ggplot(aes(x = year, y = life_expectancy,colour = country)) +
      geom_line() +
      xlab("year") +
      ylab("life_expectancy")
  })
  output$scatterPlot = renderPlot({
    # Filter year to be the input year from the slider
    gapminder %>% filter(year %in% input$year  & !is.na(fertility)) %>%
      ggplot(aes(fertility, life_expectancy, color = continent, size = population/10^6)) +
      geom_point(alpha = 0.5) +
      xlab("Fertility (Average number of children per woman)") +
      ylab("Life expectancy (Years)") +
      
      # Set x and y axis limits to keep the same for each year
      scale_x_continuous(breaks = seq(0, 9), limits = c(1, 8)) +
      scale_y_continuous(breaks = seq(30, 85, 5), limits = c(30, 85)) +
      # Make the legend titles nicer
      scale_color_discrete(name = "Continent") +
      scale_size_continuous(name = "Population in millions") +
      
      # Change the title of the plot for each year
      # Returns a character vector containing a formatted combination 
      # of text and variable values
      ggtitle(sprintf("Life expectancy vs. fertility")) +
      theme_bw()})
  output$click1_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(gg2, input$plot_click, addDist = TRUE)
  })
  
  output$brush1_info <- renderPrint({
    brushedPoints(gg2, input$plot_brush)
  })
  output$plot1 <- renderPlot({
    gapminder %>% filter(year %in% input$year1  & !is.na(infant_mortality)) %>%
      ggplot(aes(infant_mortality, life_expectancy, color = continent, size = population/10^6)) +
      geom_point(alpha = 0.5) 
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(gg, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(gg, input$plot1_brush)
  })
  
  
    
  }
  



# Run the application 
shinyApp(ui = ui, server = server)

