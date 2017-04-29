


derivS <- function( tdummy,  sdummy,  idummy,  rdummy){
      
      return(-1.0*beta*sdummy*idummy ); 
      
}
derivI <- function( tdummy,  sdummy,  idummy,  rdummy, M.mat){
      return(beta*sdummy*idummy - gamma*idummy - M.mat[0,1] + M.mat[1,0]); #add random exp
}

derivR <- function( tdummy, sdummy,  idummy,  rdummy){
      return(gamma*idummy);
}

Mrand <- function( seed,  lambda.v, t){
      
      for(int j = 0; j < nPlace; j++){
            M[j] = rpois(lambda[j]*exp(-0.05*t), seed);
      }
}
