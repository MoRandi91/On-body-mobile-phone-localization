# Features that were constructed from more simple features
# x,y,z - signals from 3-axis accelerometer
complexFeatures <- function(x, y, z)
{
  
  mx <- mean(x) 
  my <- mean(y)
  mz <- mean(z)
  
  MMV <- sqrt(mx*mx+my*my+mz*mz) # Magnitude Mean Value (MMV)
  SMA <- sum(abs(x)+abs(y)+abs(z))/length(x) # Signal Magnitude Area (SMA)
  
  # magnitude
  s <- sqrt(x^2 + y^2 + z^2)
  ftf <- mean(s) # mean
  fts <- var(s) #var
  
  AAD <- mean(abs(s - mean(s))) # Average Absolute Difference (AAD)  
  F1 <- abs(mean(s) - MMV)
  
  # Durbin-Watson autocorrelation
  ss <- s - mean(s)
  
  dwa <- 0          
  if (sum(ss * ss) > 0)
    dwa = sum(diff(ss)^2) / (sum(ss*ss))
  
  return c(MMV, SMA, AAD, F1, dwa)
}