

outlierPair <- function(x,INDEX,p=.05,na.rm=TRUE){
  
  xSplit <- split(x,INDEX)
  n <- length(x)/2

  d <- lapply(xSplit,FUN="diff")
  d <- unlist(d,use.names=FALSE)
  
  dSqMax <- max(d^2)
  fval <- dSqMax/((sum(d^2)-dSqMax)/(n-1))
  pval <- n * (1-pf(fval,1,n-1))
  test <- (pval<p)

  whichMax <- which.max(d^2)
  whichPair <- which(x %in% xSplit[[whichMax]])

  return(list(test=test,pval=pval,whichPair=whichPair))
}

## examples 
## data(estrogen)
## no single outlier
## outlierPair(exprs(estrogen)[1,],INDEX=pData(estrogen),p=.05)
## true single outlier
## outlierPair(exprs(estrogen)[247,],INDEX=pData(estrogen),p=.05)
## an outlier Pair, but would not pass madOutPair
## outlierPair(exprs(estrogen)[495,],INDEX=pData(estrogen),p=.05)







