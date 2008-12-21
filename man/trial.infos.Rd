\name{trial.infos}

\alias{trial.infos}


\title{ Extract attributes of trials }

\description{
Takes a data frame and trial ids and return a new data frame with the same
layout as the supplied one except: 1.) For each trial only the first records is
contained.  2.) Columns that vary within trials are dropped.  In other words
trial.infos return a data frame with the attributes of the trials.
}

\usage{
trial.infos(data, trial_id)
}

\arguments{
  \item{data}{is a data frame containing the eye movement data.  Each line
  represents one fixation of the eyes.  The fixations of a trial have to be
  listed en bloc and in chronological order.  The only required column is:
  trial ID (unique in the whole data set).  Typically there are more columns
  holding information regarding trials (like subject id, condition, response,
  etc.) and information regarding the fixations (duration, coordinates).  See
  the example data set provided with this package.}
  \item{trial_id}{A vector with the trial ids, typically the column of
  \code{data} that identifies trials.}
}

\details{trial.info splits \code{data} into trials and checks within each trial
which columns vary.  If a value varies the column is dropped in the whole set.
The resulting data frame can be used to color a scatter plot of scanpaths
according to trial attributes like experimental condition.}

\value{A data frame with one row for each trial and a subset of the columns in
\code{data}.}

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
trial_infos <- trial.infos(eyemovements, eyemovements$trial_id)
colors <- c("red", "blue", "green", "black")
plot(map, col=colors[trial_infos$relation])
}

\keyword{ manip }
