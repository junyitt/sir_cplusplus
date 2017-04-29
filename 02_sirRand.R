setwd("C:/Users/User/git/sir_cplusplus")
source("01_f_sir_rand.R")

library(plyr);library(dplyr);
library(reshape2)
library(ggplot2)
library(gridExtra)
library(grid)


setwd("C:/Users/User/Desktop/")
migperday <- NULL
# mig.df <- read.csv("migrate.csv", header = F)
# migperday <- mig.df/365; migperday <- as.numeric(t(as.matrix(migperday)))*0.05

t = 0;
##Parameters
#Steps
endtime <<- 120; 
tstep <- 0.05;
nsteps = endtime/tstep;

#NUMBER OF PLACES
nPlace <- 3; places.v <- 1:nPlace
M0 <- data.frame(matrix(0, nPlace, nPlace)); 

###PLACE PARAMETER #KL, SING, BANGKOK
placeName <- c("Kuala Lumpur", "Singapore", "Bangkok")
pp = c(22.2e6, 2.018e6, 62.806e6);
ii <- c(1000, 0, 0);
ss = pp-ii;
rr = c(0,0,0);

###density
# densityP <- c(67.339, 3380.235, 122.282)/pp;
densityP <- c(67.339, 100, 100)/pp/1.5;
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
      mMat <- generateM.f(nPlace = nPlace, seed = seed, time, migperday)
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
df2 <- melt(df, id.vars = c("t", "place")); df2[,"place"] <- factor(df2[,"place"], levels=c(1,2,3), labels = placeName, ordered=TRUE)
# ggplot(df2, aes(x=t, y = value, group = variable, color = variable)) + geom_point()

# ggplot(df2, aes(x=t, y = value, colour=variable)) + geom_point() + facet_grid(place~.)
L1<-lapply(unique(df2[,"place"]), FUN = function(x){
      u1 <- df2[,"place"] == x
      df3 <- df2[u1,]
      g <- ggplot(df3, aes(x=t, y = value, colour=variable)) + geom_point() + ggtitle(x)
            return(g)
})


grid.arrange(L1[[1]],L1[[2]], L1[[3]],  ncol=2)

##Write files
# K <- 1:nPlace
# lapply(K, FUN = function(num){
#       df <- a[[num]]
#       setwd("C:/Users/User/Desktop")
#       fname <- paste0("Place_", num, format(Sys.time(), "%Y%m%d_%H%M_%S"),".csv")
#       write.csv(df, fname, row.names = F)
# })

setwd("C:/Users/User/Desktop/")
      # fname <- paste0("Place_", 1, format(Sys.time(), "%Y%m%d_%H%M_%S"),".csv")
      fname <- "output_sir.csv"
      
      write.csv(fdf, fname, row.names = F)
      
      # fname2 <- "output_sir.txt"
      # write.table(x = fdf, file = fname2, sep = ",",row.names = TRUE)
      # 
      # dftest <- read.table(fname2, header = T, sep = ",")
      # 