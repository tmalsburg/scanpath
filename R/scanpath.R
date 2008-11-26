
# Author: Titus v.d. Malsburg <malsburg@gmail.com>

# Projects from plane coordinates to lat-lon on an sphere representing the
# visual field (gnomonic projection).
# See http://mathworld.wolfram.com/GnomonicProjection.html.  The formulae
# simplify because we set phi_1 and lambda_0 to 0.
visual_field <- function(x, y, center_x, center_y, viewing_distance, unit_size) {
  x <- (x - center_x) * unit_size / viewing_distance
  y <- (y - center_y) * unit_size / viewing_distance

  rho <- sqrt(x**2 + y**2)
  c <- atan(rho)

  # FIXME: at point 0,0 we get NaNs
  # NOTE: 180/pi converts radians to degrees
  sin_c <- sin(c)
  lat <- asin(y * sin_c / rho) * 180/pi
  lon <- atan(x * sin_c / (rho * cos(c))) * 180/pi

  data.frame(lat, lon)
}

# Calculates the pair-wise similarities of scanpaths using scasim:
scasim <- function(data, formula, modulator = 0.83) {
  records <- prepare_data(data, formula)
  if (length(records) == 4)
    distances(records, cscasim_wrapper)
  else if (length(records) == 3)
    distances(records, cscasim_roi_wrapper)
}

# Arranges the data in a format suitable for later processing.  (This way, we
# don't have to carry around the formula.)
prepare_data <- function(data, formula) {
  # NOTE: There's certainly (and hopefully) a more idiomatic and more elegant
  # way of doing this:
  names <- rownames(attributes(terms(formula))$factors)
  if (length(names) == 4) {
    df <- cbind.data.frame(factor(data[[names[1]]]),  # trial_id
                           data[[names[2]]],          # x-coordinate
                           data[[names[3]]],          # y-coordinate
                           data[[names[4]]])          # fixation duration
    colnames(df) <- c("trial_id", "fix_x", "fix_y", "fix_d")
  } else if (length(names) == 3) {
    df <- cbind.data.frame(factor(data[[names[1]]]),  # trial_id
                           data[[names[2]]],          # roi
                           data[[names[3]]])          # fixation duration
    colnames(df) <- c("trial_id", "roi", "fix_d")
  }
  return(df)
}

# Generic function for calculating pair-wise similarities / distances.
# NOTE: Doing this in C with threads would speedup things on multicore CPUs.
# Just partition the matrix and let a thread run over each submatrix.  We would
# have to dig into reading data.frames and the like using stuff from
# Rinternals.h, though.
distances <- function(records, fun) {
  trials <- split(records, records$trial_id, drop=TRUE)
  n <- length(trials)
  m <- matrix(nrow=n, ncol=n)

  for (i in 1:n) {
    for (j in i:n) {
      # NOTE: This is not necessarily true:
      if (i==j) {
        m[j,i] <- 0
        next
      }
      s <- trials[[i]]
      t <- trials[[j]]
      if (nrow(s)<2 || nrow(t)<2) {
        print("malformed trial")
        m[j,i] <- m[i,j] <- 0
      } else {
        m[j,i] <- m[i,j] <- fun(s, t)
      }
    }
  }
  return(m)
}

# Wrapper for the implementation in C:
# s and t are data frames holding one trial each.
cscasim_wrapper <- function(s,t) {
  .C(cscasim,
     as.integer(length(s$fix_x)),
     as.double(s$fix_x),
     as.double(s$fix_y),
     as.double(s$fix_d),
     as.integer(length(t$fix_x)),
     as.double(t$fix_x),
     as.double(t$fix_y),
     as.double(t$fix_d),
     0.83,
     result = double(1))$result
}

# Wrapper for the implementation in C:
# s and t are data frames holding one trial each.
cscasim_roi_wrapper <- function(s,t) {
  .C(cscasim_roi,
     as.integer(length(s$roi)),
     as.integer(s$roi),
     as.double(s$fix_d),
     as.integer(length(t$roi)),
     as.integer(t$roi),
     as.double(t$fix_d),
     result = double(1))$result
}

# Implementation in R, just for measuring the speed-up by using C.  The
# speed-up gained by the C implementaiton is about factor 200 to 300!
#scasim <- function (s, t) {
#
#  modulator <- 0.8
#  m <- nrow(s)
#  n <- nrow(t)
#
#  d <- matrix(nrow=m+1, ncol=n+1)
#  d[1,1] <- 0
#  acc <- 0
#  for (i in seq(1, m)) {
#    acc <- acc + s$fix_d[i]
#    d[i+1,1] <- acc
#  }
#  acc <- 0
#  for (j in seq(1, n)) {
#    acc <- acc + t$fix_d[j]
#    d[1,j+1] <- acc
#  }
#
#  for (i in seq(1, m)) {
#    for (j in seq(1, n)) {
#      dist <- sqrt((t$fix_x[j]-s$fix_x[i])**2 + (t$fix_y[j]-s$fix_y[i])**2)
#      f <- modulator**dist
#      cost <- abs(t$fix_d[j]-s$fix_d[i])*f + (t$fix_d[j]+s$fix_d[i])*(1-f)
#      d[i+1,j+1] <- min(d[i,j+1]+s$fix_d[i],
#                        d[i+1,j]+t$fix_d[j], 
#                        d[i,j]+cost)
#    }
#  }
#  return(d[m+1,n+1])
#}

