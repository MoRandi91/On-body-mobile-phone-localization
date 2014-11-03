# jerk-filter for finding features, which are independent for phone position 
# x,y,z - signals from 3-axis accelerometer, dt - time interval
jerkFilter <- function(x, y, z, dt = 1)
{
  jerk_x = x[(1+dt):length(x)] - x[1:(length(x)-dt)]
  jerk_y = y[(1+dt):length(y)] - y[1:(length(y)-dt)]
  jerk_z = z[(1+dt):length(z)] - z[1:(length(z)-dt)]
  
  windows <- ceiling(length(x)/window)
  g <- matrix(0,windows,3)
  for(i in 1:windows)
  {
    g[i,1] <- mean(x)
    g[i,2] <- mean(y)
    g[i,3] <- mean(z)
    x <- x - g[i,1]
    y <- y - g[i,2]
    z <- z - g[i,3]
  }
  
  cos_alpha = (x[(1+dt):length(x)] * x[1:(length(x)-dt)] + y[(1+dt):length(x)] * y[1:(length(y)-dt)] +
                 z[(1+dt):length(z)] * z[1:(length(z)-dt)]) / 
    sqrt(x[(1+dt):length(x)] * x[(1+dt):length(x)] + y[(1+dt):length(y)] * y[(1+dt):length(y)] +
           z[(1+dt):length(z)] * z[(1+dt):length(z)]) / 
    sqrt(x[1:(length(x)-dt)] * x[1:(length(x)-dt)] + y[1:(length(y)-dt)] * y[1:(length(y)-dt)] +
           z[1:(length(z)-dt)] * z[1:(length(z)-dt)])
  cos_alpha[cos_alpha > 1] = 1
  cos_alpha[cos_alpha < -1] = -1
  
  alpha = acos(cos_alpha) * 180 / 3.14
  a = sqrt(x*x+y*y+z*z)
  c = sqrt(jerk_x*jerk_x + jerk_y*jerk_y + jerk_z*jerk_z)
  
  n = length(x);
  sj = c;
  
  for(i in (1+dt):n)
  {
    if (a[i] < a[i-dt])
      c[i-dt] = -c[i-dt]
    sj[i-dt] = (1 + alpha[i-dt]/180)*c[i-dt]
  }
  
  return sj
}
