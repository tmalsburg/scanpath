\name{scasim}

\alias{scasim}

\title{ Calculate (dis)similarity of scanpaths }

\description{
Calculates the pair-wise differences of scanpaths using the scasim measure.
}

\usage{
scasim(data, formula, center_x, center_y, viewing_distance, unit_size, modulator = 0.83)
}

\arguments{
  \item{data}{is a data frame containing the eye movement data.  Each line
  represents one fixation of the eyes.  The fixations of a trial have to be
  listed en bloc and in chronological order.  Required columns: trial ID for
  each trial (unique in the whole data set), x-coordinate of fixations,
  y-coordinate of fixations, duration of fixations.  See the example data
  set provided with this package.}
  % TODO: discuss ROIs instead of coordinates
  \item{formula}{specifies the relevant coloumns.  The left-hand side
  specifies the column that holds trial IDs.  The right-hand side consist of
  terms that specify x-coordinate of a fixation, y-coordinate of a fixation,
  duration of a fixation, in that order.}
  \item{center_x, center_y}{is the point in the coordinate system of the data
  that is targeted when the eye looks straight ahead.  (Usually the center of
  the screen.)}
  \item{viewing_distance}{is distance of the eyes to the screen in some unit.}
  \item{unit_size}{is the size of one unit of the coordinate system compared to
  one unit in which the viewing distance was given.  So when the viewing
  distance was given in inches and the coordinate system has 60 units per inch
  (e.g. pixels on a 60 DPI screen) the unit size is 1/60.}
  \item{modulator}{specifies how spatial distances between scanpaths are
  assessed.  When set to 0, any spatial divergence of two compared scanpaths is
  penalized independently of its degree.  When set to 1, the scanpaths are
  compared only with respect to their temporal patterns.  The default value
  approximates the sensitivity to spatial distance found in the human visual
  system.}
}

% TODO the following paragraph is obsolete ... sigh
\details{In principle the x- and y-coordinates could be specified as
coordinates of the plane on which the stimulus was presented, i.e. pixels of
the screen.  However, in order to account for the higher visual acuity in the
center of the visual field the viewing distance of the eyes to the stimulus and
the unit of the coordinates have to go into the equations.  When working with
this package this is achieved by first projecting the coordinates to a
coordinate system that uses degrees of the visual field as its unit.  This
projection from the stimulus plane to the visual field (a half sphere) can be
performed with the function \code{inverse_gnomonic}.}

\value{
A matrix containing the pair-wise similarities.
}

\references{Titus von der Malsburg and Shravan Vasishth (2008) A New Method for
Analyzing Eye Movements. In Proceedings of the CUNY sentence processing
conference, North Carolina }

\author{ Titus v. d. Malsburg }

\seealso{\code{\link{cmdscale}} can be applied to the output of scasim for
fitting maps of scanpaths.

\code{\link{inverse_gnomonic}} projects coordinates from a 2D plane to degrees of the visual field.}

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
