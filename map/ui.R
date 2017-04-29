#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
step = 7
interval = 500

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Population_Disease Modelling Template"),
  
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
    # sidebarPanel(
    #           
    #    sliderInput("bins",
    #                "Time:",
    #                min = 0,
    #                max = 500,
    #                value = k)
    # 
    # ),
    
    # Show a plot of the generated distribution
    mainPanel(
          h3("Beta 2.0"),
          h4("Added: Autoplay function."),
          h4("Added: Legend with values."),
          h4("Unable to solve: Screen refreshing cannot be prevented, smooth screen updates can't be achieved."),
          
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
