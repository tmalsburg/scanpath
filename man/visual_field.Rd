\name{visual_field}

\alias{visual_field}

\title{Project coordinates from stimulus plane to degrees of the visual field}

\description{Calculates the pair-wise differences of scanpaths using the
scasim measure.}

\usage{visual_field(coordinates, viewing_distance, unit_size)}

\arguments{
  \item{coordinates}{a vector with the coordinates to project.}
  \item{viewing_distance}{the distance of the eyes to the stimulus in cm.}
  \item{unit_size}{the size of one unit of the original coordinate system in
  cm.}
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
eyemovements$fix_x_degree <- visual_field(eyemovements$fix_x, 60, 1/20)
eyemovements$fix_y_degree <- visual_field(eyemovements$fix_y, 60, 1/20)
}
\keyword{ manip }
