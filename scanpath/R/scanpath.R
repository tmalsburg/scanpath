#' This package provides tools for analyzing spatio-temporal patterns
#' in eye movements, a.k.a. scanpaths.
#'
#' @name scanpath
#' @docType package
#' @title Tools for analyzing scanpaths
#' @author Titus von der Malsburg \email{malsburg@@posteo.de}
#' @references
#' von der Malsburg, T. and Vasishth, S. (2011). What is the scanpath
#' signature of syntactic reanalysis? Journal of Memory and Language,
#' 65(2):109-127.
#' @keywords eye movements cluster scanpaths package
#' @seealso \code{\link{scasim}}

NULL

# Creates a data.frame with the same layout as the given data frame except: 1.)
# For each group (as defined by groups) only the first records is contained.
# 2.) Columns that vary within groups are dropped.  constant.vars can be used
# to extract trial infos from a data frame containing fixations or subject
# infos from a data frame containing trial infos.

constant.vars <- function (data, groups)
{

  index <- eval(substitute(index), d, parent.frame())

  stopifnot(length(index)==nrow(d))

  constant.cols <- sapply(d, function(col) {
    all(tapply(col, index, function(x) length(unique(x))==1))
  })

  d[!duplicated(index),constant.cols]
  
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

#' Given a set of scanpaths, this function calculates the similarities
#' between all pairs of scanpaths in the set.
#'
#' @title Calculate pair-wise similarities among a set of scanpaths
#' @param data is a data frame containing the eye movement data.  Each
#' line represents one fixation of the eyes.  The fixations of a trial
#' have to be listed en bloc and in chronological order.  Required
#' columns are: trial ID (unique in the whole data set), x- and y-
#' coordinate of fixations, duration of fixations.  If the targets of
#' fixations are not given as coordinates but as regions of interest
#' (ROI), there should be a factor providing the IDs of the ROIs
#' instead of the coordinates.  See the example data set provided with
#' this package.
#' @param formula tells which columns in the given data frame are
#' relevant.  The left-hand side specifies the column that holds
#' fixation duration The right-hand side consist of terms that specify
#' x- and y-coordinate of the fixations (in that order) conditioned on
#' trial IDs.  In case fixation targets are not given as coordinates
#' but as ROIs, the right-hand side specifies the column indicating
#' the ROI conditioned on trial IDs.  In the latter case it is not
#' necessary to specify the parameters \code{center_x},
#' \code{center_y}, \code{viewing_distance} and \code{unit_size}
#' because spatial relations among fixation targets are not accounted
#' for.  See examples below.
#' @param center_x, center_y is the point in the coordinate system of
#' the data that is targeted when the eye looks straight ahead
#' (usually the center of the screen).  Only relevant for
#' \code{scasim}.
#' @param viewing_distance is the distance of the eyes to the
#' screen.  Only relevant for \code{scasim}.
#' @param unit_size is ratio of one unit of the coordinate system and
#' the unit in which the viewing distance was given.  So, one unit in
#' the coordinate system is \code{unit_size * unit of
#' distance}.  Example: If the coordinates are pixels on a screen with
#' 60 dpi and the unit of the distance is inches, \code{unit_size} has
#' to be set to 1/60.  Only relevant for \code{scasim}.
#' @param modulator specifies how spatial distances between fixations
#' are assessed.  When set to 0, any spatial divergence of two
#' compared scanpaths is penalized independently of its degree.  When
#' set to 1, the scanpaths are compared only with respect to their
#' temporal patterns.  The default value approximates the sensitivity
#' to spatial distance found in the human visual system.  Only
#' relevant for \code{scasim}.
#' @param data2 optionally provides a second set of scanpaths.  In
#' this case \code{scasim} computes the distances from each scanpath
#' in the first set to each scanpath in the second set.  The resulting
#' matrix of distances is therefore not necessarily square.
#' @param formula2 specifies the relevant columns in \code{data2} in
#' case they are named differently in \code{data2} than in
#' \code{data}.  \code{formula2} defaults to \code{formula}.
#' @param normalize specifies how each calculated similarity value for
#' scanpaths s and t should be normalized.  There are three possible
#' modes:
#' 
#' \describe{
#'   \item{\code{fixations}}{normalize by dividing by the total number
#'   of fixations in s and t,}
#'   \item{\code{durations}}{divide by the total duration of s and t,}
#'   \item{\code{FALSE}}{don't normalize at all.}
#' }
#' 
#' The choice of normalization mode can have a strong effect when
#' fitting maps of scanpaths using \code{\link{isoMDS}} or
#' \code{\link{cmdscale}}.  Not normalizing at all can yield maps that
#' are difficult to interpret when scanpaths differ markedly in their
#' duration or number of fixations.
#' @details When using \code{scasim}, the dissimilarity of two
#' scanpaths is assessed based on the distances of their fixation
#' targets.  If the distances are really small, the dissimilarity will
#' mostly be a function of just the differences of the fixation
#' durations.  The evaluation of distance accounts for the particular
#' way the human visual system magnifies the center of the visual
#' field and the drop in acuity at its periphery.
#' @return A matrix containing the pair-wise dissimilarities.  Columns
#' and rows correspond to trials.  Their order is the same as the
#' order of trials in \code{data}.
#' @keywords eye movements scanpaths cluster
#' @references
#' von der Malsburg, T. and Vasishth, S. (2011). What is the scanpath
#' signature of syntactic reanalysis? Journal of Memory and Language,
#' 65(2):109-127.
#'
#' von der Malsburg, T., Vasishth, S., and Kliegl,
#' R. (2012). Scanpaths in reading are informative about sentence
#' processing. In Michael Carl, P. B. and Choudhary, K. K., editors,
#' Proceedings of the First Workshop on Eye-tracking and Natural
#' Language Processing, pages 37-53, Mumbai, India. The COLING 2012
#' organizing committee.
#' @author Titus von der Malsburg \email{malsburg@@posteo.de}
#' @seealso \code{\link{MASS::isoMDS}} can be applied to the output of
#' \code{scasim} in order to fitting maps of scanpaths.
#' @export
#' @examples
#' data(eyemovements)
#' 
#' # Calculating dissimilarities: when looking straight ahead the gaze
#' # targets the point with the coordinates (512,384), viewing
#' # distance (eye to screen) is 60 cm, 1 unit in the data (pixel) is
#' # 1/30 cm.
#'
#' dissimilarities <- scasim(eyemovements, dur ~ x + y | trial,
#'                           512, 384, 60, 1/30)
#' 
#' # Using cmdscale for fitting a map:
#'
#' map <- isoMDS(dissimilarities)
#' plot(map)
scasim <- function(data, formula, center_x, center_y, viewing_distance,
                   unit_size, modulator=0.83, data2=NULL, formula2=formula,
				           normalize="fixations")
{
  data <- prepare.data(data, formula)

  cscasim.wrapper2 <- function(s, t) cscasim.wrapper(s, t, modulator, normalize)

  if (length(data) == 4)          # Coordinates were given:
    data <- cbind(data, inverse.gnomonic(data$x, data$y, center_x,
    					 center_y, viewing_distance, unit_size))

  if (is.null(data2))
    distances(data, cscasim.wrapper2)
  else {
    data2 <- prepare.data(data2, formula2)
    if (length(data2) == 4)
      data2 <- cbind(data, inverse.gnomonic(data2$x, data2$y, center_x,
                                            center_y, viewing_distance,
                                            unit_size))
    
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
        if (nrow(s1)<1 || nrow(s2)<1) {
          warning("malformed trial")
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
        if (nrow(s1)<1 || nrow(s2)<1) {
          warning("malformed trial")
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
cscasim.wrapper <- function(s, t, modulator=0.83, normalize)
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

  if (normalize=="fixations")
    result <- result / (nrow(s) + nrow(t))
  else if(normalize=="durations")
    result <- result / (sum(s$d) + sum(t$d))
  return(result)
}

# Select mean scanpath (in a cluster):
which.mean <- function(d, select=NULL) {
  if (!is.null(select))
    d <- subset(d, rownames(d) %in% select, select=select)
  d <- rowSums(d)
  names(d)[which.min(d)]
}
