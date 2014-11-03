# Generate standart features, such as mean, sd, quantiles, ...
# s - arbitrary one-dimensional signal
genStandFeatures <- function(s) {
  feature <- matrix(0,1,44)
  
  feature[1] <- sd(s)
  feature[2] <- mean(s)
  feature[3] <- min(s)
  feature[4] <- max(s)
  q <- quantile(s, probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), names = F)
  feature[5:9] <- q[2:6]
  for(j in 1:3)
  {
    feature[9+j] <- sum(s[s < q[j]])
    feature[12+j] <- sum((s[s < q[j]])^2)
    feature[15+j] <- sum(s[s > q[j+4]])
    feature[18+j] <- sum((s[s > q[j+4]])^2)
  }
  
  p <- 1
  for(i in 1:(length(q)-1))
    for(j in (i+1):length(q))
    {
      feature[21+p] <- q[j] - q[i]
      p <- p + 1
    }
  
  # kurtosis and skewness
  feature[43] <- skewness(s) # skewness (third central moment)
  feature[44] <- kurtosis(s) # kurtosis (fourth central moment)
  
  return feature
}