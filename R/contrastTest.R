contrastTest <- function(model,lambda,cVec=NA,p=.01){
  if(is.na(cVec)) cVec <- rep(0,dim(lambda)[1])

  betahat <- coef(model)
  XpXinv <- summary(model)$cov.unscaled
  sigmahat <- summary(model)$sigma

  df1 <- dim(lambda)[1]
  df2 <- summary(model)$df[2]

  cEst <- lambda %*% betahat
  
  Fstat <- (t(cEst - cVec) %*%
             solve(lambda %*% XpXinv %*% t(lambda)) %*%
             (cEst - cVec))/(df1 * sigmahat^2)
  
  pvalue <- 1-pf(Fstat,df1,df2)
  if(pvalue<p) test <- "REJECT"
  else test <-"FAIL TO REJECT"

  return(list(test=test,Fstat=Fstat,pvalue=pvalue,cEst=cEst))
}
