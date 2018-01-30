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
#' @seealso \code{\link{scasim}}, \code{\link{avg.group.dist}}, \code{\link{constant.vars}}, \code{\link{which.centroid}}
#' @useDynLib scanpath cscasim

NULL

#' Given a data frame and a grouping variable, this function returns a
#' new data frame that contains only the variables that are constant
#' within the levels of the grouping variable.  The results data frame
#' contains only one line for each level of the grouping
#' variable.  This function can be used to extract trial-based
#' information from a data frame containing experimental data with
#' multiple measurements per trial.
#'
#' @title Extract variables that are constant within the levels of a
#' grouping variable.
#' @param data a data frame.
#' @param groups a grouping variable.  Either a vector with the same length as the data frame or simply a column in \code{data}.
#' @return A data frame with one entry for each level of the grouping variable.
#' @export 
#' @examples
#' data(eyemovements)
#' head(eyemovements)
#' constant.vars(eyemovements, trial)
constant.vars <- function (data, groups)
{

  groups <- eval(substitute(groups), data, parent.frame())

  stopifnot(length(groups)==nrow(data))

  constant.cols <- sapply(data, function(col) {
    all(tapply(col, groups, function(x) length(unique(x))==1))
  })

  data[!duplicated(groups),constant.cols]
  
}

#' Projects from points given in plane coordinates to lat-lon, i.e.,
#' to the surface of a sphere (inverse gnomonic projection).  This can
#' be used to project screen coordinates to coordinates of the visual
#' field of somebody sitting in front of the screen.
#'
#' @title Project points from a plane to a sphere.
#' @param x the x-coordinates of the points.
#' @param y the y-coordinates of the points.
#' @param center_x is the x-coodrinate of the point that is targeted
#' when the eye looks straight ahead (usually the center of the
#' screen).
#' @param center_y is the y-coodrinate of the point that is targeted
#' when the eye looks straight ahead (usually the center of the
#' screen).
#' @param distance is the distance of the plane to the center of the
#' sphere.
#' @param unit_size is ratio of one unit of the plane coordinate
#' system and the unit in which the viewing distance was given.  So,
#' one unit in the coordinate system is \code{unit_size * unit of
#' distance}.
#' @return A data frame containing lat and lon coordinates.
#' @references See
#' http://mathworld.wolfram.com/GnomonicProjection.html.
#' @export
#' @examples
#' data(eyemovements)
#' x <- eyemovements$x
#' y <- eyemovements$y
#'
#' latlon <- inverse.gnomonic(x, y, 268, 382, 8, 1/30)
#' 
#' # Before projection:
#' plot(x, y)
#' # After:
#' with(latlon, plot(lon, lat))
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
#' coordinate of fixations, and the fixation durations.  See the
#' example data set provided with this package.
#' @param formula specifies which columns in the given data frame are
#' relevant.  The left-hand side specifies the column that contains
#' the fixation duration The right-hand side consist of terms that
#' specify x- and y-coordinate of the fixations (in that order)
#' conditioned on trial IDs.  See examples below.
#' @param center_x is the x-coodrinate of the point that is targeted
#' when the eye looks straight ahead (usually the center of the
#' screen).
#' @param center_y is the y-coodrinate of the point that is targeted
#' when the eye looks straight ahead (usually the center of the
#' screen).
#' @param viewing_distance is the distance of the eyes to the
#' screen.
#' @param unit_size is ratio of one unit of the coordinate system and
#' the unit in which the viewing distance was given.  So, one unit in
#' the coordinate system is \code{unit_size * unit of
#' distance}.  Example: If the coordinates are pixels on a screen with
#' 60 dpi and the unit of the distance is inches, \code{unit_size} has
#' to be set to 1/60.
#' @param modulator specifies how spatial distances between fixations
#' are assessed.  When set to 0, any spatial divergence of two
#' compared scanpaths is penalized independently of its degree.  When
#' set to 1, the scanpaths are compared only with respect to their
#' temporal patterns.  The default value approximates the sensitivity
#' to spatial distance found in the human visual system.
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
#' fitting maps of scanpaths using \code{\link[MASS]{isoMDS}} or
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
#' @seealso \code{\link[MASS]{isoMDS}} can be applied to the output of
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
#' dissimilarities <- scasim(eyemovements, duration ~ x + y | trial,
#'                           512, 384, 60, 1/30)
#' 
#' # Using cmdscale for fitting a map:
#'
#' map <- cmdscale(dissimilarities)
#' plot(map, cex=4)
#' text(map, labels=rownames(map))
scasim <- function(data, formula, center_x, center_y, viewing_distance,
                   unit_size, modulator=0.83, data2=NULL, formula2=formula,
                   normalize="fixations")
{
  data <- prepare.data(data, formula)

  cscasim.wrapper2 <- function(s, t) cscasim.wrapper(s, t, modulator, normalize)

  data <- cbind(data, inverse.gnomonic(data$x, data$y, center_x,
                                       center_y, viewing_distance, unit_size))

  if (is.null(data2)) {
    distances(data, cscasim.wrapper2)
  } else {
    data2 <- prepare.data(data2, formula2)
    data2 <- cbind(data2, inverse.gnomonic(data2$x, data2$y, center_x,
                                           center_y, viewing_distance,
                                           unit_size))
    
    distances(data, cscasim.wrapper2, t2=data2)
  }
}

#' Calculates the mean similarities among sub-sets of scanpaths given
#' by a grouping variable.  For instance, when the groups are the
#' participants of the experiment, we get a matrix of the average
#' similarities between the scanpaths of the subjects.
#'
#' @title Mean similarities among sets of scanpaths
#' @param d a symmetric matrix of similarities as calculated by the
#' function \code{\link{scasim}} or a dist object (see
#' \code{\link{dist}}).
#' @param groups a vector that assigns each column to a group.
#' @return a matrix containing the pair-wise average similarities for
#' all groups.
#' @seealso \code{\link{scasim}}, \code{\link{dist}}
#' @export
#' @examples
#' data(eyemovements)
#' d <- scasim(eyemovements, duration ~ x + y | trial, 512, 384, 60, 1/30)
#' s <- constant.vars(eyemovements, trial)$subject
#' avg.group.dist(d, s)
avg.group.dist <- function(d, groups) {
  if (class(d)=="dist")
    d <- as.matrix(d)
  # This nifty trick was recommended to me by Chuck C. Berry:
  groups <- factor(groups, ordered=TRUE)
  mat <- model.matrix(~0+groups)
  tab <- table(groups)
  means <- (t(mat) %*% d %*% mat) / outer(tab, tab)
  
  colnames(means) <- rownames(means) <- levels(groups)
  means
}

# Arranges the data in a format suitable for later processing.  (This way, we
# don't have to carry around the formula.)
prepare.data <- function(data, formula)
{
  terms <- strsplit(deparse(formula), " [~+|] ")[[1]]
  stopifnot(length(terms) %in% 3:4)
  df <- data[terms]
  if (length(terms)==3) 
    colnames(df) <- c("duration", "x", "trial")
  else
    colnames(df) <- c("duration", "x", "y", "trial")
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
    if (length(t)==1)
      return(m)
    for (i in 1:(length(t)-1))
      for (j in (i+1):length(t))
        m[j,i] <- m[i,j] <- fun(t[[i]], t[[j]])
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
    for (i in 1:length(t))
      for (j in 1:length(t2))
        m[i,j] <- fun(t[[i]], t2[[j]])
    rownames(m) <- unique(tid)
    colnames(m) <- unique(t2id)
  }
  return(m)
}

# Wrapper for the implementation in C:
# s and t are data frames holding one trial each.
cscasim.wrapper <- function(s, t, modulator=0.83, normalize)
{
  result <- .C(cscasim,
     as.integer(length(s$lon)),
     as.double(s$lon),
     as.double(s$lat),
     as.double(s$duration),
     as.integer(length(t$lon)),
     as.double(t$lon),
     as.double(t$lat),
     as.double(t$duration),
     modulator,
     result = double(1))$result

  if (normalize=="fixations")
    result <- result / (nrow(s) + nrow(t))
  else if (normalize=="durations")
    result <- result / (sum(s$duration) + sum(t$duration))
  else if (normalize)
    stop("Unrecognized normalization parameter: ", normalize)
  result
}

#' Given a matrix of similarities, this function identifies the item
#' that has the smallest average distance to the other items.
#'
#' @title Find item with minimal average distance to other items
#' @param d a symmetric matrix of similarities or a \code{\link{dist}}
#' object.
#' @param select a vector of names of items that should be included in
#' the analysis.  These names should correspond to the row and column
#' names of \code{d}.  Items not listed are ignored completely.  The
#' default is to take all items into account.
#' @details Although the function is called which.centroid, the
#' selected item is not necessarily the centroid in the strict
#' sense.  However, it is the item which is closer to the centroid
#' that any other item in the set.
#' @return the name of the item that has the minimal average distance
#' to the other items.
#' @export
#' @examples
#' data(eyemovements)
#' d <- scasim(eyemovements, duration ~ x + y | trial, 512, 384, 60, 1/30)
#' which.centroid(d)
#' which.centroid(d, c("1", "2", "3"))
which.centroid <- function(d, select=NULL) {
  if (!is.null(select)) {
    t <- rownames(d) %in% select
    d <- d[t,t]
  }
  d <- rowMeans(d)
  names(d)[which.min(d)]
}

#' This data set consists of nine trials.  One of these trials was
#' recorded in an experiment where the participants had to read single
#' sentences on a screen, one at a time.  The other eight trials are
#' modifications of the recorded trial.  There are three imaginary
#' participants and for each participant there are three trials: in
#' the first trial the participant read the sentence straight from
#' left to right. In the second trial there was a short regression
#' from the last word to the beginning of the sentence.  In the third
#' trial the regression was longer and the eyes check several
#' intermediate words before returning to the end of the sentence.  In
#' order to simulate different reading speeds, the recorded fixation
#' durations were modified by adding additional time that was sampled
#' from a normal distribution with different means for each subject.
#'
#' @title Fixational eye movements during reading a sentence
#' @name eyemovements
#' @docType data
#' @usage data(eyemovements)
#' @format In the data frame each row represents one fixation of the
#'   eyes.  Fixations are ordered chronologically within trial.  The
#'   data frame has the following columns:
#'   \describe{
#'   \item{\code{subject}}{the id of the subject}
#'   \item{\code{trial}}{the id of the trial}
#'   \item{\code{word}}{the word that was targeted by the fixation}
#'   \item{\code{x}}{the x coordinate of a fixation in pixels}
#'   \item{\code{y}}{the y coordinate of a fixation in pixels}
#'   \item{\code{duration}}{the duration of a fixation in milliseconds}}
#' @keywords datasets

NULL
