
madOutPair <- function(x,whichPair,c=4){

  lb <- median(x)-c*mad(x)
  ub <- median(x)+c*mad(x)

  test1 <- (x[whichPair[1]] > ub) |  (x[whichPair[1]] < lb)
  test2 <- (x[whichPair[2]] > ub) |  (x[whichPair[2]] < lb)

  singleOut <- "NA"	  
  if(xor(test1,test2)){
    singleOut <- whichPair[which.max(c(test1,test2))]
  }

  return(singleOut)
    
}

  
