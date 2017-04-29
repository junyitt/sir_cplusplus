#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

df33 <- data.frame(name = c("B", "F", "Rock"),
                   pop = sqrt(c(600000, 50000, 1e6)),
                   lat = c(39.292, 39.414, 39.084),
                   lng = c(-76.607, -77.420, -77.153)
)

df1 <- df33 %>% leaflet() %>% setView(lat = 39, lng = -77, zoom = 7) %>% addTiles() 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
      #temp####
      # symbolurl <- "http://www.clipartkid.com/images/256/clipart-biohazard-symbol-clipart-best-clipart-best-FMw9Ll-clipart.png"
      # myicon <- makeIcon( 
      #       iconUrl = symbolurl,
      #       iconWidth = 31*215/230, iconHeight = 31,
      #       iconAnchorX = 31*215/230/2, iconAnchorY = 16
      #       
      # )
      # 
      # 
      # pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
      # popouttext <- c("danger1")
      ########
      
      # addMarkers(icon = myicon, popup = popouttext, clusterOptions = markerClusterOptions()) %>%
      
      # addRectangles(lat1 = 37.3858, lng1 = -77,
      #               lat2 = 38, lng2 = -79)  %>%
      # observe({
      # val = 12;
      # updateSliderInput(session, "time", value = val,
      #                   min = floor(val/2), max = val+4, step = (val+1)%%2 + 1)
      # })
      
      # df2 <- reactive({
      #       k2 <- input$bins 
      #       CLICK <- F
      #       
      #       # observeEvent(input$recalc, {
      #       #       for(k in 1:500){
      #                   df1 %>% addCircles(lng = ~lng, lat = ~lat, weight = 0.5, radius  = df33$pop*k2, color = "red", fillOpacity = 0.3)
      #       #       }
      #       #       CLICK <- T;     
      #       # })
      #       
      #       # if(!CLICK){
      #       #       df1 %>% addCircles(lng = ~lng, lat = ~lat, weight = 0.5, radius  = df33$pop*k, color = "red", fillOpacity = 0.3)
      #       # }
      #       # 
      #      
      # })
      # 
      
      # df4 <- df33 %>% leaflet() %>% addTiles() %>% addLegend(labels = LETTERS[1:3], colors = c("green", "red", "blue"))
      
      
  output$mymap <- renderLeaflet({
        k2 <- input$bins
     
        df1 %>% addCircles(lng = ~lng, lat = ~lat, weight = 0.5, radius  = df33$pop*k2, color = "red", fillOpacity = 0.3)%>% 
              addLegend(labels = paste0("Infected: ", formatC(200*k2^2, digit = 10, big.mark = ",")), colors = "red")
        
        
  })
  
})
