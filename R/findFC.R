
findFC <- function(model,lambdaNum,lambdaDenom,logbase=NULL){

  if(is.null(logbase)){
	num <- lambdaNum %*% model$coef
  	denom <- lambdaDenom %*% model$coef
  	FC <- num/denom
  }

  else {
	
	lambdaDiff <- lambdaNum-lambdaDenom	

	if(logbase=="exp"){
		FC <- exp(lambdaDiff %*% model$coef)
	}
	else FC <- logbase^(lambdaDiff %*% model$coef)
  }

  return(FC)
}
