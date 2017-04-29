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
nPlace <- 2; places.v <- 1:nPlace
M0 <- data.frame(matrix(0, nPlace, nPlace)); 

###PLACE PARAMETER
pp = c(1e6, 1e6);
ss = c(1e6-1000, 1e6);
ii <- c(1000, 0);
rr = c(0,0);

bbeta = c(0.65/pp[1], 0.4/pp[2])
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
      mMat <- generateM.f(nPlace = 2, seed = seed, time)
      gdf[,8:(nPlace+8-1)] <- mMat
    
      sirplace.list <- lapply(1:nPlace, FUN = function(place){
            g <- as.numeric(gdf[place,])
            rk4(G = g, tstep = tstep, M = mMat, row = place)
      })
      
      gdf <<- rbind.fill(sirplace.list)
      
      return(gdf)
})

fdf <<- do.call(rbind, AA)


df <- fdf[1:5]
df2 <- melt(df, id.vars = c("t", "place")); df2[,"place"] <- as.factor(df2[,"place"])
# ggplot(df2, aes(x=t, y = value, group = variable, color = variable)) + geom_point()
ggplot(df2, aes(x=t, y = value, colour=variable)) + geom_point() + facet_grid(place~.)

##Write files
# K <- 1:nPlace
# lapply(K, FUN = function(num){
#       df <- a[[num]]
#       setwd("C:/Users/User/Desktop")
#       fname <- paste0("Place_", num, format(Sys.time(), "%Y%m%d_%H%M_%S"),".csv")
#       write.csv(df, fname, row.names = F)
# })

setwd("C:/Users/User/Desktop")
      fname <- paste0("Place_", 1, format(Sys.time(), "%Y%m%d_%H%M_%S"),".csv")
      write.csv(fdf, fname, row.names = F)


