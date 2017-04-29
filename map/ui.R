
library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
step = 2
interval = 750

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Miracle Map - Visualizing SIR Model "),
  
    
    # Show a plot of the generated distribution
    mainPanel(
          leafletOutput("mymap"),
          
          column(10,
          sliderInput("bins", "Time:",
                      min = 0, max = 150, value = 0, step = step,
                      animate=animationOptions(interval=interval, loop=F)
                      
          )
          ), 
          
          
          # actionButton("recalc", "Random Locations!"),
          p()
          
    )
  # )
))
