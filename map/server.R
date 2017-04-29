# setwd("C:/Users/User/git/sir_cplusplus")
# source("02_sirRand.R")

library(shiny)
library(leaflet)

# setwd("C:/Users/User/Desktop")
urlf <- "https://raw.githubusercontent.com/junyitt/sir_cplusplus/master/data/output_sir.csv"

library(curl)
fdf <- read.csv( curl(urlf) )
nPlace <- length(unique(fdf[,"place"]))
fdf[,"lgi"] <- 1e4*round(log(1+fdf[,"i"]),0)*1.3

shinyServer(function(input, output, session) {
      
  output$mymap <- renderLeaflet({
        time <- input$bins
        u1 <- fdf[, "t"] > time
        df11 <- fdf[u1,][1:nPlace,]
        
        #temp
        df22 <- data.frame(df11, lng = c(101.6869, 103.8198, 100.5018) , lat = c(3.1390, 1.3521, 13.7563))
        df22[,"place"] <- c("Kuala Lumpur", "Singapore", "Bangkok")
            
            
        df1 <- df22 %>% leaflet() %>% setView(lat = 8, lng = 102, zoom = 5) %>% addTiles() 
        inft <- as.numeric(df22[,"i"]);
        place.legend <- c("MYS: ", "SGP: ", "THA: ")
        df1 %>% addCircles(lng = ~lng, lat = ~lat, weight = 0.5, radius  = ~lgi, color = "red", fillOpacity = 0.3) %>% 
              addLegend(title = "Number of Infected", labels = paste0(place.legend, formatC(round(inft,0), digit = 10, big.mark = ",")), colors = rep("red",3)) 
              # addLegend(labels = paste0("Infected: ", formatC(, digit = 10, big.mark = ",")), colors = "red")
        
        
  })
  
})
