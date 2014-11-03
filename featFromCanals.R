# Correlation-features from signal
# Input: magnutide - magnitude signal, ah - horizontal projection, 
# av - vertical projection
featFromCanals <- function(magnitude, ah, av)
{
  feat <- matrix(1,3)
  feat[1] <- cor(mag, ah)
  feat[2] <- cor(mag, av)
  feat[3] <- cor(ah, av)
  return feat
}
