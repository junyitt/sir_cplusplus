# setwd("C:/Users/User/git/sir_cplusplus")
# source("02_sirRand.R")
{
      lambda.f <-function(nPlace, time){
            runif(nPlace^2, 10,100)*exp(-0.05*time);
      }
      
      generateM.f <- function(nPlace, seed, time){
            set.seed(seed)
            k <- rpois(n = nPlace^2, lambda = lambda.f(nPlace, time))
            mMat <- matrix(k, nrow = nPlace, ncol = nPlace, byrow = T)
            diag(mMat) <- 0
            return(mMat)
      }
      
      
      rk4 <- function(G, tstep, M, row){
            t <- G[1];
            s <- G[2];
            i <- G[3];
            r <- G[4];
            betap <- G[6]
            gammap <- G[7]
            
            h = tstep/2.0;
            
            s1 = tstep * derivS(t, s, i, r,betap, gammap);
            i1 = tstep * derivI(t, s, i, r, M, row,betap, gammap);
            r1 = tstep * derivR(t, s, i, r, betap, gammap);
            
            s2 = tstep * derivS(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0, betap, gammap);
            i2 = tstep * derivI(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0, M, row, betap, gammap);
            r2 = tstep * derivR(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0, betap, gammap);
            
            s3 = tstep * derivS(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0, betap, gammap);
            i3 = tstep * derivI(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0, M, row, betap, gammap);
            r3 = tstep * derivR(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0, betap, gammap);
            
            s4 = tstep * derivS(t+tstep, s+s3, i+i3, r+r3, betap, gammap);
            i4 = tstep * derivI(t+tstep, s+s3, i+i3, r+r3, M, row, betap, gammap);
            r4 = tstep * derivR(t+tstep, s+s3, i+i3, r+r3, betap, gammap);
            
            s = s + (s1 + (2.0*(s2 + s3)) + s4)/6.0;
            i = i + (i1 + (2.0*(i2 + i3)) + i4)/6.0;
            r = r + (r1 + (2.0*(r2 + r3)) + r4)/6.0;
            
            if(i < 0.5){ i <- 0 }
            
            dff <- t(c(t,s,i,r, G[5:length(G)]))
            dff<-as.data.frame(dff)             
            colnames(dff) <- fdfcolName
            
            return(dff)
            
      }
      
      
      derivS <- function( tdummy,  sdummy,  idummy,  rdummy, betap, gammap){
            
            return(-1.0*betap*sdummy*idummy ); 
            
      }
      derivI <- function( tdummy,  sdummy,  idummy,  rdummy, M.mat, row, betap, gammap){
            return(betap*sdummy*idummy - gammap*idummy - sum(M.mat[row,]) + sum(M.mat[,row])); #add random exp
      }
      derivR <- function( tdummy, sdummy,  idummy,  rdummy, betap, gammap){
            return(gammap*idummy);
      }
      
      
      serialNext = function(prefix){
            if(!file.exists(prefix)){return(prefix)}
            i=1
            repeat {
                  f = paste(prefix,i,sep=".")
                  if(!file.exists(f)){return(f)}
                  i=i+1
            }
      }
      
      
}

{
      library(plyr);library(dplyr);
      library(reshape2)
      library(ggplot2)
      
      t = 0;
      ##Parameters
      #Steps
      endtime <- 240; 
      tstep <- 0.05;
      nsteps = endtime/tstep;
      
      #NUMBER OF PLACES
      nPlace <- 3; places.v <- 1:nPlace
      M0 <- data.frame(matrix(0, nPlace, nPlace)); 
      
      ###PLACE PARAMETER #KL, SING, BANGKOK
      pp = c(22.2e6, 2.018e6, 62.806e6);
      ii <- c(1000, 0, 0);
      ss = pp-ii;
      rr = c(0,0,0);
      
      ###density
      densityP <- c(67.339, 3380.235, 122.282)/pp;
      proportion <- 100
      ###
      # bbeta = c(0.65/pp[1], 0.4/pp[2], 0.5/pp[3])
      bbeta <- densityP/proportion
      ggamma = 0.33333; 
      
      
      
      tInt <-seq(from=0+tstep, to=endtime, by = tstep)
      
      gdf0 <- data.frame(t = 0, s = ss, i = ii, r = rr, place = places.v, betap = bbeta, gammap = ggamma, M0)
      
      fdf <<- gdf0
      fdfcolName <<- c("t", "s", "i", "r", "place", "betap", "gammap", paste0("M", 1:nPlace))
      colnames(fdf) <- fdfcolName
      
      gdf <<- fdf
      AA <- lapply(tInt, FUN = function(time){
            gdf[,1] <- gdf[,1]+tstep
            #Generate M and replace gdf 6:6+n-1
            seed <- ceiling(runif(1,1,100000));
            mMat <- generateM.f(nPlace = nPlace, seed = seed, time)
            gdf[,8:(nPlace+8-1)] <- mMat
            
            sirplace.list <- lapply(1:nPlace, FUN = function(place){
                  g <- as.numeric(gdf[place,])
                  rk4(G = g, tstep = tstep, M = mMat, row = place)
            })
            
            gdf <<- rbind.fill(sirplace.list)
            
            return(gdf)
      })
      
      fdf <<- do.call(rbind, AA)
}

library(shiny)
library(leaflet)

# fdf
setwd("C:/Users/User/Desktop")
fdf <- read.csv("Place_120170429_1817_14.csv")



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
        df1 %>% addCircles(lng = ~lng, lat = ~lat, weight = 0.5, radius  = ~2*i, color = "red", fillOpacity = 0.3) %>% 
              addLegend(labels = paste0("Infected: ", formatC(round(inft,0), digit = 10, big.mark = ",")), colors = rep("red",3)) 
              # addLegend(labels = paste0("Infected: ", formatC(, digit = 10, big.mark = ",")), colors = "red")
        
        
  })
  
})
