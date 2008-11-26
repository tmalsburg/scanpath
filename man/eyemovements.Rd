\encoding{utf-8}
\name{eyemovements}
\alias{eyemovements}
\title{Eyetracking data from one subject solving a geometrical puzzle}
\description{
	The \code{eyemovements} data frame has 150 trials of one subject that has
	to recognize geometrical relations between two sets of shapes.
}
\format{
	In this data frame each row represents one fixation of the eyes.  The data
	frame has, among others, the following columns:
	\describe{
	\item{vpn}{the id of the subject}
	\item{trial\_id}{the id of the trial}
	\item{item}{the id of the item}
	\item{fix\_x}{the x coordinate of a fixation in pixels}
	\item{fix\_y}{the y coordinate of a fixation in pixels}
	\item{fix\_d}{the duration of a fixation in milliseconds}
	}
}
%\details{}
\source{Franziska Preuße, Humboldt University, Berlin}
\keyword{datasets}
