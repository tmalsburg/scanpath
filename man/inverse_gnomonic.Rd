\name{inverse_gnomonic}

\alias{inverse_gnomonic}

\title{Project coordinates from stimulus plane to degrees of the visual field}

\description{Performs an inverse gnomonic projection from a plane to a
hemisphere, which represents our case the retina or the visual field.  The
resulting coordinates are given as latitude and longitude.}

\usage{inverse_gnomonic(x, y, center_x, center_y, distance, unit_size)}

\arguments{
  \item{x, y}{are vectors with the coordinates to project.}
  \item{center_x, center_y}{is the point in the coordinate system of the data
  where the hemisphere is nearest (or where the hemisphere touches the plane).}
  \item{distance}{is distance of the (sphere) center of the hemisphere to the
  plane.}
  \item{unit_size}{is the size of one unit of the coordinate system compared to
  one unit in which the viewing distance was given.  So when the 
  distance was given in inches and the coordinate system has 60 units per inch
  the unit size is 1/60.}
}

%\details{ }

\value{A vector with the projected coordinates.}

\references{Wikipedia explains the relevant math:
http://en.wikipedia.org/wiki/Trigonometric\_function\#Tangent }

\author{ Titus v.d. Malsburg }

\seealso{ See also \code{\link{scasim}}.}

\examples{
data(eyemovements)
# Projecting to degrees: viewing distance is 60 cm, 1 unit in
# the data (pixel) is 1/20 cm.
#eyemovements$fix_x_degree <- inverse_gnomonic(eyemovements$fix_x, 60, 1/20)
#eyemovements$fix_y_degree <- inverse_gnomonic(eyemovements$fix_y, 60, 1/20)
}
\keyword{ manip }
