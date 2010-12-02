\encoding{utf8}

\name{match.scanpath}

\alias{match.scanpath}


\title{ Find Particular Fixation Patterns Using Regular Expressions } 


\description{This function can be used to extract specific fixation patterns
from a set of eye movements.  The desired pattern can be specified using regular
expressions allowing a flexible and powerful way to specify complex patterns.}

\usage{
match.scanpath(syms, trial, expr, nth=1, subpattern=0)
}

\arguments{
  \item{syms}{a vector of symbols for regions of interest that describes the
  sequence of eye movements.  The n-th element in \code{syms} indicated which
  region of interest was gazed at during the n-th fixation.}
  \item{trial}{a vector of trial ids.  Must have the same length as
  \code{syms}.  This vector is used to delimit scanpaths from different
  trials.}
  \item{expr}{a regular expression that describes the fixation pattern that
  should be matched.  Uses PCRE syntax.}
  \item{nth}{if there are several matches in a trial, this integer indicates
  which match should be returned.}
  \item{subpattern}{if there are subpatterns in \code{expr} (a.k.a. groups),
  this integer indicates which groups should be matched.  If 0 is specified, the
  whole pattern is matched.}
}

\value{A vector of indices for syms that indicates which fixations are belonging
to the matched pattern.}

\author{Titus von der Malsburg <malsburg@uni-potsdam.de>, Tobias Günther <toguenth@uni-potsdam.de>}

\examples{
data(eyemovements)

plot.scanpaths(dur~word|trial, eyemovements)

# Finds fixation sequences starting on word 9 followed by a
# left-wards saccade, followed by anything:
idx <- with(eyemovements,
            match.scanpath(word, trial, "9[1-8][1-9]*"))

plot.scanpaths(dur~word|trial, eyemovements[idx,])

# Finds arbitrary fixation sequences following a back and forth movement on
# region 9:
idx <- with(eyemovements,
            match.scanpath(word, trial, "9[1-8]([1-9]+)",
                           subpattern=1))

plot.scanpaths(dur~word|trial, eyemovements[idx,])
}

\keyword{ manip }
