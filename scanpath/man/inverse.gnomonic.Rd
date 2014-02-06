\name{inverse.gnomonic}

\alias{inverse.gnomonic}

\title{Project Coordinates From a Plane to a Hemisphere}

\description{Performs an inverse gnomonic projection, i.e. a projection from a
plane to a hemisphere of which the plane is a tangent.  The resulting
coordinates are given as latitude and longitude.}

\usage{inverse.gnomonic(x, y, center_x, center_y, distance, unit_size = 1)}

\arguments{
  \item{x, y}{are vectors with the coordinates to project.}
  \item{center_x, center_y}{is the point on the plane where the hemisphere
  touches the plane.}
  \item{distance}{is the distance of the center of the sphere to the plane.}
  \item{unit_size}{if the unit of the coordinates is different from the unit of
  the distance, \code{unit_size} specifies their ratio.  One unit in the
  coordinate system is \code{unit_size * unit of distance}.  Example: If the
  coordinates are pixels on a screen with 60 dpi and the unit of the distance
  is inches, \code{unit_size} has to be set to 1/60.}
}

%\details{ }

\value{A data frame with columns \code{lat} and \code{lon}}

\references{Wikipedia explains the relevant math:
http://en.wikipedia.org/wiki/Trigonometric\_function\#Tangent }

\author{Titus von der Malsburg <malsburg@uni-potsdam.de>}

\seealso{See also \code{\link{scasim}}.}

\examples{
data(eyemovements)
# Projecting eye tracking data to the visual field.  The coordinates are pixels
# of the screen on which the stimuli were presented.  The viewing distance was
# 60 cm, the size of one pixel was 1/30 cm.
ll <- inverse.gnomonic(eyemovements$x, eyemovements$y,
                       512, 384, 60, 1/30)
plot(ll$lon, ll$lat)
}
\keyword{ manip }
