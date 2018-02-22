pcre <- function(pattern, text, subpattern) {
  x <- gregexpr(pattern, text, perl=TRUE, useBytes=FALSE)
  if (subpattern==0) {
    x
  } else {
    lapply(x, function(i) {
      r <- as.vector(attr(i, "capture.start")[,subpattern])
      attr(r, "match.length") <- as.vector(attr(i, "capture.length")[,subpattern])
      r
    })
  }
}

#' Finds fixations embedded in a context that is specified using a
#' regular expression.
#'
#' @title Find specific fixations
#' @param l a vector of single letters or single-digit integers each
#'   representing a fixation on a region of interest.
#' @param groups a grouping variable indicating to which scanpath each
#'   fixation belongs.  The search for matching patterns in performed
#'   within each group separately and matches crossing group boundaries
#'   are not considered.
#' @param expr a regular expression describing the scanpath pattern of
#'   interest.
#' @param nth specifies which match should be returned when there are
#'   multiple within a group.  The default is to return all matches.
#' @param subpattern the subpattern of interest.  If zero, the
#'   beginnings of the full patterns will be returned.  If n>0, the
#'   beginnings of the n-th subpatterns will be returned.
#' @return A vector giving the indices of the matches in l.
#' @export
#' @examples
#' data(eyemovements)
#' words <- eyemovements$word
#' trial <- eyemovements$trial
#'
#' # Find fixations on word 6 (which is represented by letter "f"):
#' idx <- find.fixation(words, trial, "6")
#' eyemovements[idx,]
#'
#' # Find fixations on word 6 but only the second match within
#' # a group (i.e., trial):
#' idx <- find.fixation(words, trial, "6", nth=2)
#' eyemovements[idx,]
#'
#' # Find fixations on word 6 that are followed by fixations on word 7:
#' idx <- find.fixation(words, trial, "67")
#' eyemovements[idx,]
#'
#' # Find those fixations on word 6 that are preceded by fixations on
#' # word 5:
#' idx <- find.fixation(words, trial, "5(6)", subpattern=1)
#' eyemovements[idx,]
#'
#' # Find the beginning of a second sweep over the sentence:
#' idx <- find.fixation(words, trial, "[123]+[456]+[78]+([123]+[456]+[78]+)", subpattern=1)
#' eyemovements[idx,]
find.fixation <- function(l, groups, expr, nth=NA, subpattern=0) {
  
  l <- as.character(l)
  if (!all(nchar(l)==1)) {
    stop("Some region identifiers have more (or less) digits than one: ",
         paste(unique(l)[nchar(unique(l))!=1], collapse=", "))
  }

  stopifnot(is.na(nth)||floor(nth)==nth)
  stopifnot(is.na(nth)||nth>0)

  s <- tapply(l, groups, paste, collapse="")

  hits <- pcre(expr, s, subpattern)
  offsets <- c(0, cumsum(nchar(s)[-length(s)]))
  names(offsets) <- unique(groups)
  # Remove no-match entries:
  hits <- lapply(hits, function(h) if (h[[1]]!=-1) h)
  if (!is.na(nth))
    hits <- lapply(hits, function(h) if (length(h)>=nth) h[[nth]])
  # Count matches per string:
  nm <- sapply(hits, length)
  # Repeat offsets of string with several matches:
  offsets <- rep(offsets, nm)
  
  hits <- unlist(hits)

  hits + offsets

}

#' Identifies sequences of fixations that match a pattern specified
#' using a regular expression.
#'
#' @title Identify scanpath patterns
#' @param l a vector of single letters or single-digit integers each
#'   representing a fixation on a region of interest.
#' @param groups a grouping variable indicating to which scanpath each
#'   fixation belongs.  The search for matching patterns in performed
#'   within each group separately and matches crossing group
#'   boundaries are not considered.
#' @param expr a regular expression describing the scanpath pattern of
#'   interest.
#' @param subpattern the subpattern of interest.  If zero, the
#'   complete match will be marked.  If n>0, only the n-th subpattern
#'   will be marked.  See examples.
#' @return A vector giving the indices of the matching
#'   fixations.  Only the first match within each scanpath will be
#'   marked.
#' @export
#' @examples
#' data(eyemovements)
#' words <- eyemovements$word
#' trial <- eyemovements$trial
#'
#' # Scanpaths from the last word until the end of the trial:
#' idx <- match.scanpath(words, trial, "8.+")
#' scanpaths1 <- eyemovements[idx,]
#' plot_scanpaths(scanpaths1, duration~x|trial)
#'
#' # Scanpaths from third word until a fixation on the second word:
#' idx <- match.scanpath(words, trial, "3.+2")
#' scanpaths2 <- eyemovements[idx,]
#' plot_scanpaths(scanpaths2, duration~x|trial)
#'
#' # Find scanpaths from the third word until the end of the trial but
#' # only if they contain a fixation on the second word:
#' idx <- match.scanpath(words, trial, "3.+2.*")
#' scanpaths3 <- eyemovements[idx,]
#' plot_scanpaths(scanpaths3, duration~x|trial)
#'
#' # Find scanpaths spanning words 6, 7, and 8 but only those that
#' # are directly preceded by a fixation on word 4:
#' idx <- match.scanpath(words, trial, "4([678]+)", subpattern=1)
#' scanpaths4 <- eyemovements[idx,]
#' plot_scanpaths(scanpaths4, duration~word|trial)
match.scanpath <- function(l, groups, expr, subpattern=0) {

  l <- as.character(l)
  if (!all(nchar(l)==1)) {
    stop("Some region identifiers have more (or less) digits than one: ",
         paste(unique(l)[nchar(unique(l))!=1], collapse=", "))
  }

  s <- tapply(l, groups, paste, collapse="")

  hitsg <- pcre(expr, s, subpattern)
  hits <- sapply(hitsg, function(x) x[[1]])

  if (all(hits==-1)) {
    warning("No fixations match the specified pattern.")
    return(integer(0))
  }

  mlen <- sapply(hitsg, function(x) attr(x, "match.length")[[1]])
  offsets <- c(0, cumsum(nchar(s)))[1:length(hits)]

  offsets <- offsets[hits!=-1]
  mlen <- mlen[hits!=-1]
  hits <- hits[hits!=-1]

  hite <- hits + mlen + offsets - 1
  hits <- hits + offsets

  unlist(lapply(1:length(hits), function(i) hits[i]:hite[i]))

}

