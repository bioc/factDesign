findFC <- function(model,lambdaNum,lambdaDenom,logbase=NULL){
  if(is.null(logbase)){
	num <- lambdaNum %*% coef(model)
  	denom <- lambdaDenom %*% coef(model)
  	FC <- num/denom
  }
  else {	
	lambdaDiff <- lambdaNum-lambdaDenom	

	if(logbase=="exp"){
		FC <- exp(lambdaDiff %*% coef(model))
	}
	else FC <- logbase^(lambdaDiff %*% coef(model))
  }

  return(FC)
}
