# Help function for finding vertical and gorizonatal projections
# Find projection b on the line a
prOntoLine <- function(a, b)
{
  P <- sum(a * b) / sum(a * a)
  p <- P * a
  p
}

# Find vertical and gorizonatal projections
# x,y,z - vectors with signals on 3-axis accelerometer
projections <- function(x, y, z)
{
  acc <- rbind(x, y, z)
  rownames(acc) <- c("vertical x", "vertical y", "vertical z")
  gravity <- rowMeans(acc)
  acc <- apply(acc, 2, function(x) x - gravity)
  v <- apply(acc, 2, function(x) prOntoLine(gravity, x))
  h <- acc - v
  av <- v
  horizontal <- sqrt(colSums(h^2))
  
  return rbind(horizontal, av)
}
