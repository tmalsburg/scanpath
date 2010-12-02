\encoding{utf8}

\name{find.fixation}

\alias{find.fixation}


\title{ Find Fixations that Occur in Particular Contexts Using Regular Expressions } 


\description{This function finds fixations that occur in specific contexts
of eye movements.  This context can be specified using regular expressions
allowing a flexible and powerful way to specify complex patterns.}

\usage{
find.fixation(syms, trial, expr, nth=1, subpattern=0)
}

\arguments{
  \item{syms}{a vector of symbols for regions of interest that describes the
  sequence of eye movements.  The n-th element in \code{syms} indicated which
  region of interest was gazed at during the n-th fixation.}
  \item{trial}{a vector of trial ids.  Must have the same length as
  \code{syms}.  This vector is used to delimit scanpaths from different
  trials.}
  \item{expr}{a regular expression that describes the fixation pattern that
  should be searched for.  Uses PCRE syntax.}
  \item{nth}{if there are several matches in a trial, this integer indicates
  which match should be returned.}
  \item{subpattern}{if there are subpatterns in \code{expr} (a.k.a. groups),
  this integer indicates which groups should be matched.  If 0 is specified, the
  whole pattern is matched.}
}

\value{A vector of integers that indicate the position of the matching fixations
in syms.}

\author{Titus von der Malsburg <malsburg@uni-potsdam.de>, Tobias Günther <toguenth@uni-potsdam.de>}

\examples{
data(eyemovements)

# Finds fixations on word 9 followed by a
# left-wards saccade, followed by anything:
idx <- with(eyemovements,
            find.fixation(word, trial, "9[1-8][1-9]*"))
eyemovements[idx,]

# Finds arbitrary fixations following a
# back and forth movement on region 9:
idx <- with(eyemovements,
            find.fixation(word, trial, "9[1-8]([1-9]+)",
                           subpattern=1))
eyemovements[idx,]

}

\keyword{ manip }
