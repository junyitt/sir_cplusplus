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

