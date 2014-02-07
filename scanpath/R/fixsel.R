pcre <- function(pattern, text, subpattern)
{

  stopifnot(as.integer(subpattern)==subpattern)
  .Call("_gregexpr", pattern, text, subpattern)

}

#' Given a vector of values \code{x} and a set of replacement values
#' \code{r} (one for each unique value in \code{x}), this function
#' replaces each value in \code{x} by the corresponding new value in
#' \code{r}.
#'
#' @title Replace all values in a vector by other values
#' @param x a vector.
#' @param r a vector of replacement values.  One value for each
#' element in \code{dot(unique(x))}.
#' @param res.type a function for converting the result vector to a
#' desired data type.
#' @param na.value a replacement value for NAs.
#' @param ... extra arguments that are passed to \code{res.type}.
#' @return a version of \code{x} in which all original values are
#' substituted by the replacements specified in \code{r}.
#' @note This function can be used to replace arbitrary region
#' identifiers in fixation data by single letters.  This format is
#' needed by the functions \code{\link{find.fixation}} and
#' \code{\link{match.scanpath}}.
#' @export
#' @examples
#' replace.all(1:10, letters[1:10])
#' replace.all(10:1, letters[1:10])
#'
#' data(eyemovements)
#' words <- eyemovements$word
#'
#' replace.all(words, letters[1:length(unique(words))])
#' replace.all(words, letters[1:length(unique(words))], paste, collapse="")
replace.all <- function(x, r, res.type=as.character, na.value=NA, ...) {

  stopifnot(length(unique(r))==length(r))
  stopifnot(length(unique(x[!is.na(x)]))==length(r))

  x <- factor(x, labels=r)
  if (!is.na(na.value))
    x <- suppressWarnings(NAToUnknown(x, na.value))
  res.type(x, ...)

}

#' This function uses regular expressions to identify elements in a
#' vector of letters where a specific pattern begins.  Among other
#' things, this can be used to identify certain eye movement patterns
#' in fixation data.
#'
#' @title Mark the beginning of specified pattern in a letter sequence
#' @param l a vector of single characters.
#' @param groups a grouping variable.  The search for matching
#' patterns in performed in each group separately.  Matches crossing
#' group boundaries are not considered.
#' @param expr a regular expression describing the pattern.
#' @param nth specifies which match should be returned if there were
#' several within a group.  The default is to return all matches.
#' @param subpattern the subpattern of interest.  If zero, the
#' beginning of the full pattern will be marked.  If n>0, the
#' beginning of the n-th subpattern will be marked.  See examples.
#' @return A vector giving the indices in l where the matches
#' begin.  If subpattern > 0, then the indices will point to the
#' beginning of the subpatterns.
#' @export
#' @examples
#' data(eyemovements)
#' words <- eyemovements$word
#' l <- replace.all(words, letters[1:length(unique(words))])
#' trial <- eyemovements$trial
#' # Find fixations on word 6 (which is represented by letter "f"):
#' idx <- find.fixation(l, trial, "f")
#' words[idx]
#' # Find fixations on word 6 but only the second match within
#' # a group (i.e., trial):
#' idx <- find.fixation(l, trial, "f", nth=2)
#' words[idx]
#' # Find fixations on word 6 that are followed by fixations on word 7 ("g"):
#' idx <- find.fixation(l, trial, "fg")
#' words[idx]
#' # Find those fixations on word 6 that followed fixations on word 5 ("e"):
#' idx <- find.fixation(l, trial, "e(f)", subpattern=1)
#' words[idx]
#' # Find the beginning of a second sweep over the sentence:
#' idx <- find.fixation(l, trial, "a[b-h]+(a+[b-g]+)", subpattern=1)
#' words[idx]
find.fixation <- function(l, groups, expr, nth=NA, subpattern=0) {
  
  stopifnot(all(nchar(l)==1))
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

match.scanpath <- function(l, groups, expr, subpattern=0) {

  s <- tapply(l, groups, paste, collapse="")

  hitsg <- pcre(expr, s, subpattern)
  hits <- sapply(hitsg, function(x) x[[1]])
  mlen <- sapply(hitsg, function(x) attr(x, "match.length")[[1]])
  offsets <- c(0, cumsum(nchar(s)))[1:length(hits)]

  offsets <- offsets[hits!=-1]
  mlen <- mlen[hits!=-1]
  hits <- hits[hits!=-1]

  hite <- hits + mlen + offsets - 1
  hits <- hits + offsets

  # Probably not very efficient:
  f <- function(i) { hits[i]:hite[i] }
  unlist(lapply(1:length(hits), f))

}

# p <- function(x) plot.scanpaths(dur~wn|id, d[x,])
# 
# # Extract sub-scanpaths where first E and then F is fixated:
# p(extract.fix(d$wn, d$id, "EF"))
# 
# # More examples:
# p(extract.fix(d$wn, d$id, "EF*"))
# p(extract.fix(d$wn, d$id, "EF+"))
# 
# # Straight left-to-right reading:
# p(extract.fix(d$wn, d$id, "A+B+C+D+E+F+G+"))
# 
# # Regression starting on F:
# p(extract.fix(d$wn, d$id, "F[A-E]+[FG]"))

