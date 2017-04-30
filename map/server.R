# setwd("C:/Users/User/git/sir_cplusplus")
# source("02_sirRand.R")

library(shiny)
library(leaflet)
library(ggplot2)

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
  
  
  output$leafl <- renderUI({
        if(!is.null(input$GetScreenHeight)){
              width  <- session$clientData$output_image1_width
              print(session$clientData)
              height <- session$clientData$output_image1_height
              leafletOutput("mymap", width = "100%", height = input$GetScreenHeight)
        }
  })
  
  dff2 <- fdf[,c(1,3,5)]
  placeName <- c("Kuala Lumpur", "Singapore", "Bangkok")
  dff2[,"place"] <- factor(dff2[,"place"], levels=c(1,2,3), labels = placeName, ordered=TRUE)
  g0 <- ggplot() + geom_point(data = dff2, aes(x=t, y = i, group=place, colour = place)) +
        scale_colour_manual(breaks = dff2$place, 
                            values = c("#f4b942","#ef53d5", "#f45541"))

  
  output$plot1 <- renderPlot({
        
        df <- fdf[,c(1,3,5)]
        time <- input$bins
        u1 <- df[, "t"] > time
        df2 <- df[u1,][1:3,]
            df2[,"place"] <- factor(df2[,"place"], levels=c(1,2,3), labels = placeName, ordered=TRUE)
       # x1 <- as.numeric(df2[1,1]); y1 <- as.numeric(df2[1,2])
        
      g <- g0  + geom_vline(xintercept=time, linetype="dashed") + ggtitle("Number of Infected")
      # + geom_point(data = df2, mapping = aes(x = t, y = i, color = place), cex = 5) 
      g
        
  })
  
  
})
