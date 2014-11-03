#Help function for finding 
#cr1 - absolute value how many signal crossing specified level 
#cr2 - normalized cr1 on length of signal
#Input: s - signal, level - specified quantile(ranges from [0,1])
crossingRate <- function(s, level)
{
  border <- quantile(s, level)
  s2 <- s - border
  
  cr1 = sum((s2[1:(length(s2)-1)]*s2[2:length(s)])<0)
  cr2 = cr1 / length(s);
  
  return c(cr1,cr2)
}

# Find crossing-rate features from signal s
# Used quatiles: 5%, 10%, 25%, 50%, 75%, 90%, 95% 
crFeatures <- function(s)
{
  probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
  res <- matrix(nrow = length(probs), ncol = 2)
  p <- 0
  feat <- matrix(nrow = 1, ncol = 28)
  
  for(i in 1:length(probs))
  {
    res[i,] <- crossingRate(s, probs[i])
    feat[i] <- res[i,2]
  }
  
  p <- length(probs) + 1
  for(i in 1:(length(probs)-1))
    for(j in (i+1):length(probs))
    {
      feat[p] <- res[i,1] / res[j,1]
      p <- p + 1
    }
  
  return feat
}