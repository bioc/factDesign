
kRepsOverA <- function(k,A=100,INDEX)
{
  function(x){
    temp <- tapply(x,INDEX,FUN=mean)
    ans <- sum(temp > A) >= k
    return(ans)
  }
}
