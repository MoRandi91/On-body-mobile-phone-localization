# Hjorth (EEG) features according to paper
# "EEG analysis based contributions on time domain properties"
HjorthFeatures <- function(s, lag = 1)
{
  
  d1 = diff(s, lag)  
  d2 = diff(d1, lag)
  
  m0 = mean(s^2)
  m1 = mean(d1^2)  
  m2 = mean(d2^2)
  
  
  ACTIVITY = m0
  MOBILITY = sqrt(m1/m0)
  COMPLEXITY = (sqrt(m2/m1)/ MOBILITY)
  
  return c(m1, m2, ACTIVITY, MOBILITY, COMPLEXITY)
}