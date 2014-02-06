\encoding{utf8}

\name{replace.syms}

\alias{replace.syms}


\title{ Replaces All Values in a Vector. }


\description{This function can be used to extract specific fixation patterns
from a set of eye movements.  The desired pattern can be specified using regular
expressions allowing a flexible and powerful way to specify complex patterns.}

\usage{
replace.syms(x, r, res.type=as.character, na.value=NA)
}

\arguments{
  \item{x}{a vector of arbitrary values.}
  \item{r}{replacement values.  One element for every element in
  \code{sort(unique(x))}.}
  \item{res.type}{a function for converting a vectir to the desired data type of
  the result vector.}
  \item{na.value}{a value used instead of NAs in the result vector.}
}

\value{A vector of the same length as \code{x} but with all values replaced by
those specified in \code{r}.}

\author{Titus von der Malsburg <malsburg@uni-potsdam.de>}

\examples{

replace.syms(c(1:10, NA), letters[1:10], as.character, na.value="x")

}

\keyword{ manip }
