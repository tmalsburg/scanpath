

# Implementation in R, just for measuring the speed-up by using C.  The
# speed-up gained by the C implementation is about factor 200 to 300!
scasim <- function (s, t) {

  modulator <- 0.8
  m <- nrow(s)
  n <- nrow(t)

  d <- matrix(nrow=m+1, ncol=n+1)
  d[1,1] <- 0
  acc <- 0
  for (i in seq(1, m)) {
    acc <- acc + s$fix_d[i]
    d[i+1,1] <- acc
  }
  acc <- 0
  for (j in seq(1, n)) {
    acc <- acc + t$fix_d[j]
    d[1,j+1] <- acc
  }

  for (i in seq(1, m)) {
    for (j in seq(1, n)) {
      dist <- sqrt((t$fix_x[j]-s$fix_x[i])**2 + (t$fix_y[j]-s$fix_y[i])**2)
      f <- modulator**dist
      cost <- abs(t$fix_d[j]-s$fix_d[i])*f + (t$fix_d[j]+s$fix_d[i])*(1-f)
      d[i+1,j+1] <- min(d[i,j+1]+s$fix_d[i],
                        d[i+1,j]+t$fix_d[j], 
                        d[i,j]+cost)
    }
  }
  return(d[m+1,n+1])
}
