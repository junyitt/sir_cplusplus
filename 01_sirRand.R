library(plyr);library(dplyr);
library(reshape2)
library(ggplot2)
endtime <- 120; 
tstep <- 0.05;
nsteps = endtime/tstep;

pp = c(1e6, 1e6);
ss = pp*0.999;
ii = pp*0.001;
rr = c(0,0);
      bbeta = 0.95/pp
      ggamma = 0.33333; 
t = 0;
nPlace <- 2;

tInt <-seq(from=0, to=endtime, by = tstep)

gdf0 <- data.frame(t = 0, s = ss, i = ii, r = rr)

df_t <<- gdf0



a <-lapply(1:nPlace, FUN = function(row){
#track migration
dfmat <<- data.frame()
cname.dfmat <- paste0("M", row, 1:nPlace);
      # g <- as.numeric(df_t[row,2:4])
      g <- as.numeric(df_t[row,1:4])
      betap <<- bbeta[row]
      gammap <<- ggamma[row]
      sir_list <- lapply(tInt, FUN = function(x){
            g[1] <- g[1] + tstep
            seed <- ceiling(runif(1,1,100000));
            mMat <- generateM.f(nPlace = 2, seed = seed, g[1])
                  dfmat <<- rbind(dfmat, mMat[row,])
            # g <<- rk4(g[2:4],x, tstep, mMat);
            
            g <<- rk4(g, tstep, mMat);
            return(g)
      })
      df <- as.data.frame(do.call(rbind, sir_list))
      colnames(df) <- c("time", "s", "i","r")
      colnames(dfmat) <- cname.dfmat
      #combine
      df2 <- data.frame(df, dfmat)
      return(df2)
})

df <- a[[1]]
df2 <- melt(df, id.vars = "time")
ggplot(df2, aes(x=time, y = value, group = variable, color = variable)) + geom_point()


