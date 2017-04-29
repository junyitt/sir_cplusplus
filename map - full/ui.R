
library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
step = 1
interval = 750

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Population_Disease Modelling Template"),
  
    
    # Show a plot of the generated distribution
    mainPanel(
          leafletOutput("mymap"),
          
          column(10,
          sliderInput("bins", "Time:",
                      min = 0, max = 252, value = 0, step = step,
                      animate=animationOptions(interval=interval, loop=F)
                      
          )
          ), 
          
          
          # actionButton("recalc", "Random Locations!"),
          p()
          
    )
  # )
))
