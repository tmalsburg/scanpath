\name{scasim}

\alias{scasim}

\title{ Calculate (dis)similarity of scanpaths }

\description{
Calculates the pair-wise dissimilarities of scanpaths using the Scasim measure.
}

\usage{
scasim(data, formula, center_x, center_y, viewing_distance, unit_size, modulator
= 0.83, data2=NULL, formula2=formula, normalize="fixations")
}

\arguments{
  \item{data}{is a data frame containing the eye movement data.  Each line
  represents one fixation of the eyes.  The fixations of a trial have to be
  listed en bloc and in chronological order.  Required columns are: trial ID
  (unique in the whole data set), x- and y- coordinate of fixations,
  duration of fixations.  If the targets of fixations are not given as
  coordinates but as regions of interest (ROI), there should be a factor
  providing the IDs of the ROIs instead of the coordinates.  See the example
  data set provided with this package.}
  \item{formula}{tells which columns in the given data frame are relevant.  The
  left-hand side specifies the column that holds fixation duration  The
  right-hand side consist of terms that specify x- and y-coordinate of the
  fixations (in that order) conditioned on trial IDs.  In case fixation targets
  are not given as coordinates but as ROIs, the right-hand side specifies the
  column indicating the ROI conditioned on trial IDs.  In the latter case it is
  not necessary to specify the parameters \code{center_x}, \code{center_y},
  \code{viewing_distance} and \code{unit_size} because spatial relations among
  fixation targets are not accounted for.  See examples below.}
  \item{center_x, center_y}{is the point in the coordinate system of the data
  that is targeted when the eye looks straight ahead (usually the center of
  the screen).  Only relevant for \code{scasim}.}
  \item{viewing_distance}{is the distance of the eyes to the screen.  Only
  relevant for \code{scasim}.}
  \item{unit_size}{is ratio of one unit of the coordinate system and
  the unit in which the viewing distance was given.  So, one unit in the
  coordinate system is \code{unit_size * unit of distance}.  Example: If the
  coordinates are pixels on a screen with 60 dpi and the unit of the distance
  is inches, \code{unit_size} has to be set to 1/60.  Only relevant for
  \code{scasim}.}
  \item{modulator}{specifies how spatial distances between fixations are
  assessed.  When set to 0, any spatial divergence of two compared scanpaths is
  penalized independently of its degree.  When set to 1, the scanpaths are
  compared only with respect to their temporal patterns.  The default value
  approximates the sensitivity to spatial distance found in the human visual
  system.  Only relevant for \code{scasim}.}
  \item{data2}{optionally provides a second set of scanpaths.  In this case
  \code{scasim} computes the distances from each scanpath in the first set to
  each scanpath in the second set.  The resulting matrix of distances is
  therefore not necessarily square.}
  \item{formula2}{specifies the relevant columns in \code{data2} in case they
  are named differently in \code{data2} than in \code{data}.  \code{formula2}
  defaults to \code{formula}.}
  \item{normalize}{specifies how each calculated similarity value for scanpaths
  s and t should be normalized.  There are three possible modes:
  
  \describe{
	\item{\code{fixations}}{normalize by dividing by the total number of
	fixations in s and t,}
	\item{\code{durations}}{divide by the total duration of s and t,}
	\item{\code{FALSE}}{don't normalize at all.}}

  The choice of normalization mode can have a strong effect when fitting maps
  of scanpaths using \code{\link{isoMDS}} or \code{\link{cmdscale}}.  Not
  normalizing at all can yield maps that are difficult to interpret when
  scanpaths differ markedly in their duration or number of fixations.
  }
}

\details{When using \code{scasim}, the dissimilarity of two scanpaths
is assessed based on the distances of their fixation targets.  If the
distances are really small, the dissimilarity will mostly be a function of just
the differences of the fixation durations.  The evaluation of distance accounts
for the particular way the human visual system magnifies the center of the
visual field and the drop in acuity at its periphery.

A detailed description of the similarity measure can be found here:
http://www.ling.uni-potsdam.de/~malsburg/malsburg\_CUNY2008.pdf

A tutorial for using the measure can be found here:
http://www.ling.uni-potsdam.de/~malsburg/malsburg\_CUNY2009.pdf
}

\value{A matrix containing the pair-wise dissimilarities.  Columns and rows
correspond to trials.  Their order is the same as the order of trials in
\code{data}.}

\references{Titus von der Malsburg and Shravan Vasishth (2008): A New Method for
Analyzing Eye Movements. In Proceedings of the CUNY sentence processing
conference, North Carolina

Titus von der Malsburg, Shravan Vasishth (2009): Analyzing Spatio-Temporal
Patterns in Eye Movements: A Method and Software. In Proceedings of the CUNY
sentence processing conference, Davis, California }

\author{Titus von der Malsburg <malsburg@uni-potsdam.de>}

\seealso{\code{\link{cmdscale}} can be applied to the output of \code{scasim}
for fitting maps of scanpaths.

\code{\link{inverse.gnomonic}} projects coordinates from a 2D plane to degrees of the visual field.}

\examples{
data(eyemovements)

# Calculating dissimilarities: when looking straight ahead the gaze targets the
# point with the coordinates (512,384), viewing distance (eye to screen) is 60
# cm, 1 unit in the data (pixel) is 1/20 cm.
dissimilarities <- scasim(eyemovements, dur ~ x + y | trial,
                          512, 384, 60, 1/30)

# Using cmdscale for fitting a map:
map <- cmdscale(dissimilarities)
plot(map)
}

\keyword{ cluster }
\keyword{ ts }
\keyword{ htest }
