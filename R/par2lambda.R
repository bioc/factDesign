
par2lambda <- function(betaNames,betas,coefs){

  lambda <- matrix(0,length(betas),length(betaNames))
  colnames(lambda) <- betaNames
  for (i in 1:length(betas)){

	lambda[i,betas[[i]]] <- coefs[[i]]
  
  }	
  
  return(lambda)
}

