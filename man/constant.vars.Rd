\name{constant.vars}

\alias{constant.vars}


\title{ Extract Values From Data Frames That Do not Vary Within Groups }

\description{
Takes a data frame and a grouping variable and creates a new data frame with the
same layout as the given data frame except: 1.) Columns with values that vary
within groups are dropped, 2.) for each group there will be only be one row, and
3.) there's an additional column named \code{#} that indicates how many rows the
group has in the original data frame.}


\usage{
constant.vars(data, groups)
}

\arguments{
  \item{data}{is a data frame containing the eye movement data.  Each line
  represents one fixation of the eyes.  The fixations of a trial have to be
  listed en bloc and in chronological order.  The only required column is:
  trial ID (unique in the whole data set).  Typically there are more columns
  holding information regarding trials (like subject id, condition, response,
  etc.) and information regarding the fixations (duration, coordinates).  See
  the example data set provided with this package.}
  \item{groups}{A vector with the trial ids, typically the column of
  \code{data} that identifies trials.}
}

\details{ In the context of fixational eye movement data this function can be
used to extract trial-specific information from a data frame containing
fixations.  For this purpose the grouping variable would be the trial ID.
Likewise, subject specific information can be extracted be using the subject ID
as the grouping variable.}

\value{A data frame with one row for each group in \code{groups} and the subset
of the columns in \code{data} that do not vary within groups.  Additionally
there's a column named \code{#} that indicates how many rows the groups
comprised in the original data frame.}

\author{Titus von der Malsburg <malsburg@uni-potsdam.de>}

\examples{
data(eyemovements)

# Calculating dissimilarities: when looking straight ahead the gaze targets the
# point with the coordinates (512,384), viewing distance (eye to screen) is 60
# cm, 1 unit in the data (pixel) is 1/20 cm.
dissimilarities <- scasim(eyemovements, trial_id ~ fix_x + fix_y + fix_d,
                          512, 384, 60, 1/20)

# Using cmdscale for fitting a map:
map <- cmdscale(dissimilarities)

# Getting trial infos:
trial_infos <- constant.vars(eyemovements, trial_id)
colors <- c("red", "blue", "green", "black")
plot(map, col=colors[trial_infos$relation])
}

\keyword{ manip }
