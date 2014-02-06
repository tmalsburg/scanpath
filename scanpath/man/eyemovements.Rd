\encoding{utf-8}
\name{eyemovements}
\alias{eyemovements}
\title{Synthetic Set of Fixational Eye Movements During Reading a Sentence}
\description{
  This data set consists of nine trials.  One trial was actually recorded in a
  real experiment where the subjects had to read single sentences, one at a
  time.  The other eight trials are modifications of the recorded trial.  There
  are three imaginary subject and for each subject there are three trials: in
  the first trial the subject reads the sentence straight from left to right.
  In the second trial there is a short regression of the eyes from the last word
  to the beginning of the sentence.  In the third trial the regression is longer
  and the eyes check several of the previous words before returning to the end
  of the sentence.  In order to simulate different reading speed of the
  subjects the recorded fixation durations were modified by adding additional
  time that was sampled from a normal distribution with different standard
  deviation for each subject.
}

\usage{data(eyemovements)}

  \format{
	In this data frame each row represents one fixation of the eyes.  The data
	frame has the following columns:
	\describe{
	\item{subject}{the id of the subject}
	\item{trial\_id}{the id of the trial (unique only within subjects)}
	\item{dur}{the duration of a fixation in milliseconds}
	\item{x}{the x coordinate of a fixation in pixels}
	\item{y}{the y coordinate of a fixation in pixels}
	\item{trial}{the id of the trial (unique in the whole data set)}
	\item{word}{the word that was targeted by the fixation}
	}
  }
%\details{}
%\source{}
\keyword{datasets}
