
findFC <- function(model,lambdaNum,lambdaDenom){

  num <- lambdaNum %*% model$coef
  denom <- lambdaDenom %*% model$coef

  FC <- num/denom
  return(FC)
}
