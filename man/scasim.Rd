\name{scasim}

\alias{scasim}
\alias{scasim.roi}


\title{ Calculate (dis)similarity of scanpaths }

\description{
Calculates the pair-wise dissimilarities of scanpaths using the Scasim measure.
}

\usage{
scasim(data, formula, center_x, center_y, viewing_distance, unit_size, modulator = 0.83)

scasim.roi(data, formula)
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
  left-hand side specifies the column that holds trial IDs.  The right-hand
  side consist of terms that specify x- and y-coordinate and duration of the
  fixations, in that order.  In case fixation targets are ROIs, the right-hand
  side consist of a term for the IDs of the ROIs and a term specifying the
  column providing the fixation durations.}
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
}

\details{When using \code{scasim}, the dissimilarity of two scanpaths
is assessed based on the distances of their fixation targets.  If the
distances are really small, the dissimilarity will mostly be a function of just
the differences of the fixation durations.  The evaluation of distance accounts
for the particular way the human visual system magnifies the center of the
visual field and the drop in acuity at its periphery.  When using
\code{scasim.roi} any such considerations are not incorporated.}

\value{A matrix containing the pair-wise dissimilarities.}

\references{Titus von der Malsburg and Shravan Vasishth (2008) A New Method for
Analyzing Eye Movements. In Proceedings of the CUNY sentence processing
conference, North Carolina }

\author{Titus von der Malsburg <malsburg@uni-potsdam.de>}

\seealso{\code{\link{cmdscale}} can be applied to the output of \code{scasim}
for fitting maps of scanpaths.

\code{\link{inverse.gnomonic}} projects coordinates from a 2D plane to degrees of the visual field.}

\examples{
data(eyemovements)

# Calculating dissimilarities: when looking straight ahead the gaze targets the
# point with the coordinates (512,384), viewing distance (eye to screen) is 60
# cm, 1 unit in the data (pixel) is 1/20 cm.
dissimilarities <- scasim(eyemovements, trial_id ~ fix_x + fix_y + fix_d,
                          512, 384, 60, 1/20)

# Using cmdscale for fitting a map:
map <- cmdscale(dissimilarities)
plot(map)
}

\keyword{ cluster }
\keyword{ ts }
\keyword{ htest }
