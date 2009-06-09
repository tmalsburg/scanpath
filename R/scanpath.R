
# Author: Titus v.d. Malsburg <malsburg@uni-potsdam.de>
# Written in 2008 and 2009.

# Creates a data.frame with the same layout as the given data frame except: 1.)
# For each group (as defined by groups) only the first records is contained.
# 2.) Columns that vary within groups are dropped.  constant.vars can be used
# to extract trial infos from a data frame containing fixations or subject
# infos from a data frame containing trial infos.

constant.vars <- function (data, groups)
{

  groups <- eval(substitute(groups), data, parent.frame())

  # First figure out which columns do not vary
  # within a trial:

  groups <- split(data, groups, drop=TRUE)
  all.equal <- function(x) all(x==x[[1]])
  cols.homogenous <- function(d) sapply(d, all.equal)
  homogeneity <- data.frame(t(sapply(groups, cols.homogenous)))
  constant.cols <- sapply(homogeneity, all)
  varying.cols <- colnames(data)[!constant.cols]

  if (all(constant.cols)) {
    data["#"] <- 1
    return(data)
  }

  # Create new data frame with one record per group and only fields that have
  # constant values within each group:

  data_molten <- melt(data, measure.vars=varying.cols)
  data_molten <- subset(data_molten, variable==varying.cols[1])
  constant.vars <- cast(data_molten, ... ~ ., fun.aggregate=length)
  constant.vars$variable <- NULL
  no_fields <- length(colnames(constant.vars))
  colnames(constant.vars)[no_fields] <- "#"
  return(constant.vars)
}

# Projects from plane coordinates to lat-lon on an sphere representing the
# visual field (inverse gnomonic projection).
# See http://mathworld.wolfram.com/GnomonicProjection.html.
inverse.gnomonic <- function(x, y, center_x, center_y, distance,
                             unit_size = 1)
{
  x <- (x - center_x) * unit_size / distance
  y <- (y - center_y) * unit_size / distance

  rho <- sqrt(x**2 + y**2)
  c <- atan(rho)

  # FIXME: at point 0,0 we get NaNs
  # NOTE: 180/pi converts radians to degrees
  # NOTE: The formulae simplify because we set phi_1 and lambda_0 to 0.  See
  # the above-mentioned link.
  sin_c <- sin(c)
  lat <- asin(y * sin_c / rho) * 180/pi
  lon <- atan2(x * sin_c, rho * cos(c)) * 180/pi
  data.frame(lat, lon)
}

# Convenience wrapper for inverse.gnomonic:
visual.field <- function(data, center_x, center_y, viewing_distance,
                          unit_size)
{
    latlon <- inverse.gnomonic(data$x, data$y, center_x, center_y,
                               viewing_distance, unit_size)
    data$lat <- latlon$lat
    data$lon <- latlon$lon
    return(data)
}

# Calculates the pair-wise similarities of scanpaths using scasim:

#scasim(eyemovements, d~x+y|trial, 512, 384, 60, 1/20)

scasim <- function(data, formula, center_x, center_y, viewing_distance,
                   unit_size, modulator=0.83, data2=NULL, formula2=formula)
{
  data <- prepare.data(data, formula)

  cscasim.wrapper2 <- function(s, t) cscasim.wrapper(s, t, modulator)

  if (length(data) == 4)          # Coordinates were given:
    data <- visual.field(data, center_x, center_y, viewing_distance, unit_size)

  if (is.null(data2))
    distances(data, cscasim.wrapper2)
  else {
    data2 <- prepare.data(data2, formula2)
    if (length(data2) == 4)
      data2 <- visual.field(data2, center_x, center_y, viewing_distance,
                            unit_size)
    distances(data, cscasim.wrapper2, t2=data2)
  }
}

# Calculates the mean similarities of sub-sets of scanpaths as given by a
# grouping variable.  For instance it groups is the subject id we get a matrix
# of the similarities of subjects.
set.scasim <- function(data, formula, sets, ...) {
  sets <- factor(sets, ordered=TRUE)
  # This nifty power move comes from Chuck C. Berry:
  mat <- model.matrix(~0+sets)
  tab <- table(sets)
  d <- scasim(data, formula, ...)
  means <- (t(mat) %*% d %*% mat) / outer( tab, tab )

  id <- diag(length(levels(sets)))
  means <- ifelse(id, 0, means)
  colnames(means) <- rownames(means) <- levels(sets)
  return(means)
}


# Arranges the data in a format suitable for later processing.  (This way, we
# don't have to carry around the formula.)
prepare.data <- function(data, formula) {
  terms <- strsplit(deparse(formula), " [~+|] ")[[1]]
  # TODO: stopifnot
  df <- data[terms]
  if (length(terms) == 4)
    colnames(df) <- c("d", "x", "y", "trial")
  else if (length(terms) == 3)
    colnames(df) <- c("d", "roi", "trial")
  df
}

# Generic function for calculating pair-wise similarities / distances.
# NOTE: Doing this in C with threads would speedup things on multicore CPUs.
# Just partition the matrix and let a thread run over each submatrix.  We would
# have to dig into reading data.frames and the like using stuff from
# Rinternals.h, though.
distances <- function(t, fun, t2=NULL) {

  # Case where only one set of trials is given:
  if (is.null(t2)) {
    tid <- t$trial
    t$trial <- NULL                 # Is supposed to make split faster.
    t <- split.data.frame(t, tid, drop=TRUE)
    m <- matrix(0, nrow=length(t), ncol=length(t))
    for (i in 1:length(t)) {
      s1 <- t[[i]]
      for (j in i:length(t)) {
        s2 <- t[[j]]
        if (nrow(s1)<2 || nrow(s2)<2) {
          print("malformed trial")
          m[j,i] <- m[i,j] <- NA
        } else if (i==j) {
          m[j,i] <- m[i,j] <- 0
        } else {
          m[j,i] <- m[i,j] <- fun(s1, s2)
        }
      }
    }
    colnames(m) <- rownames(m) <- unique(tid)
  # Case where two sets of trials are processed:
  } else {
    tid <- t$trial
    t2id <- t2$trial
    t$trial <- NULL
    t2$trial <- NULL
    t <- split.data.frame(t, tid, drop=TRUE)
    t2 <- split.data.frame(t2, t2id, drop=TRUE)
    m <- matrix(0, nrow=length(t), ncol=length(t2))
    for (i in 1:length(t)) {
      s1 <- t[[i]]
      for (j in 1:length(t2)) {
        s2 <- t2[[j]]
        if (nrow(s1)<2 || nrow(s2)<2) {
          print("malformed trial")
          m[i,j] <- NA
        } else {
          m[i,j] <- fun(s1, s2)
        }
      }
    }
    rownames(m) <- unique(tid)
    colnames(m) <- unique(t2id)
  }
  return(m)
}


# Wrapper for the implementation in C:
# s and t are data frames holding one trial each.
cscasim.wrapper <- function(s, t, modulator=0.83)
{
  if ("roi" %in% colnames(s)) {
    s$lon <- s$roi
    t$lon <- t$roi
    s$lat <- double(length(s$lon))
    t$lat <- double(length(t$lon))
    modulator <- 0
  }
  result <- .C(cscasim,
     as.integer(length(s$lon)),
     as.double(s$lon),
     as.double(s$lat),
     as.double(log(s$d)),
     as.integer(length(t$lon)),
     as.double(t$lon),
     as.double(t$lat),
     as.double(log(t$d)),
     modulator,
     result = double(1))$result
  result / (nrow(s) + nrow(t))
}


# Implementation in R, just for measuring the speed-up by using C.  The
# speed-up gained by the C implementation is about factor 200 to 300!
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

#
#  Plotting:
#

splice <- function(x, y) {
  xy <- as.vector(rbind(x, y))
  if (is.factor(x) && is.factor(y))
    xy <- factor(xy, levels=1:length(levels(x)), labels=levels(x))
  xy
}

plot.scanpaths <- function(formula, data, groups=NULL, panel=panel.scanpath,
                           auto.key=if(!is.null(groups)) list(columns = 2, lines=TRUE),
                           ...)
{

  groups <- eval(substitute(groups), data, parent.frame())
  data <- prepare.data(data, formula)

  args <- list(...)
  terms <- strsplit(deparse(formula), " [~+|] ")[[1]]
  xlab <- terms[[2]]

  if (length(data) == 3) {
    colnames(data)[[2]] <- "x"
    plot.func <- plot.scanpaths.1d
    ylab <- "time"
  } else {
    plot.func <- plot.scanpaths.2d
    ylab <- terms[[3]]
  } 

  if (!"xlab" %in% names(args))
    args$xlab <- xlab
  if (!"ylab" %in% names(args))
    args$ylab <- ylab
  
  do.call(plot.func, c(list(data, "groups"=groups, "panel"=panel,
                            auto.key=auto.key), args))
}

# TODO: Fix plots when ROIs are factor (currently it drops unused rois and puts
# wrong labels on the x-axis).
panel.scanpath <- function(x, y, groups=NULL, subscripts=NULL, ...) {

  if (!is.null(groups) & !is.null(subscripts)) {
    group <- as.integer(groups[subscripts][1])
    colors <- trellis.par.get("superpose.line")$col
    col <- colors[[((group-1)%%length(colors))+1]]
    panel.lines(x, y, col=col, ...)
  } else
    panel.lines(x, y, ...)
}

plot.scanpaths.2d <- function(data, groups=NULL, panel=panel.scanpath,
                              xlim=c(min(data$x), max(data$x)),
                              ylim=c(min(data$y), max(data$y)),
                              ...)
{
  xyplot(y~x|trial, data, panel=panel, xlim=xlim, ylim=ylim,
         groups=groups, drop.unused.levels=list(cond=TRUE, data=FALSE), ...)
}

plot.scanpaths.1d <- function(data, groups=NULL, panel=panel.scanpath,
                              ylim=NULL, ...)
{

  # Create timeline y-axis from fixation durations:

  l <- levels(data$trial)
  trial <- as.factor(splice(data$trial, data$trial))
  levels(trial) <- l

  if (!is.null(groups)) 
    groups <- splice(groups, groups)
  x <- splice(data$x, data$x)
  d <- splice(data$d, data$d)
  s <- c(TRUE, diff(as.integer(as.factor(trial))) != 0)
  d <- ifelse(s, 0, d)
  idxs <- setdiff(2:nrow(data)*2, which(s)+1)

  # CHECK: Is it possible to use filter or something similar here?
  for (i in idxs) {
    d[[i-1]] <- d[[i-2]]
    d[[i]] <- d[[i]] + d[[i-1]]
  }

  if (is.null(ylim))
    ylim <- c(min(d), max(d))
    
  xyplot(d~x|trial, data.frame(x=x, d=d, trial=trial),
         panel=panel, ylim=ylim, groups=groups, drop.unused.levels=list(cond=TRUE, data=FALSE), ...)
}

