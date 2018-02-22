
#' This function plots a set of scanpaths, each scanpath in a separate
#' panel.
#'
#' @param data is the data frame containing the variables used in the
#'   formula.
#' @param formula specifies the format of the plots.  For example,
#'   \code{dur ~ word | trial} puts words on the x-axis and time
#'   on the y-axis.  Each trial is plotted in its own panel.  This
#'   assumes that words are numbered in ascending order.  Another
#'   example: \code{dur ~ x + y | trial} plots a circle for each
#'   fixation.  The location of the circle is determined by the x- and
#'   y-coordinates of the fixation and its size (area) by the
#'   duration.  Other variables of interest can be used instead of
#'   duration, for example pupil size.
#' @param groups a variable used to distinguish different groups of
#'   scanpaths by color.  Can be used to color-code conditions or
#'   participants.
#' @return A ggplot object.  Can be modified before display, for
#'   example, by adding axis limits, labels, and titles, etc.  See
#'   examples below.
#' @export
#' @examples
#' data(eyemovements)
#' plot_scanpaths(eyemovements, duration ~ word | trial)
#' plot_scanpaths(eyemovements, duration ~ word | trial, subject)
#' plot_scanpaths(eyemovements, duration ~ x + y | trial)
#' plot_scanpaths(eyemovements, duration ~ x + y | trial, subject)
#' p <- plot_scanpaths(eyemovements, duration ~ x + y | trial, subject)
#' p + ggplot2::xlim(0, 600) + ggplot2::ylim(284, 484)
plot_scanpaths <- function(data, formula, groups=NULL) {

  if(is.data.frame(formula) || inherits(data, "formula"))
    stop("Order of arguments has changed: data goes first, the formula second (for consistency with the tidyverse framework). Sorry for the inconvenience.")

  groups <- eval(substitute(groups), data, parent.frame())
  data <- prepare.data(data, formula)
  if (is.null(groups)) {
    data$groups <- as.integer(1)
  } else {
    data$groups <- groups
  }

  terms <- strsplit(deparse(formula), " [~+|] ")[[1]]

  if (length(terms)==3) {
    p <- plot_scanpaths.1d(data, terms) + ggplot2::labs(y="Time")
  } else if (length(terms)==4) {
    p <- plot_scanpaths.2d(data, terms)
  } else {
    stop("Formula for plotting has incorrect syntax.")
  }

  p <- p + ggplot2::theme(legend.position = "top")

  if(length(unique(data$groups))>1)
    p <- p + ggplot2::scale_colour_discrete(name = "Group:")

  p
}

plot_scanpaths.1d <- function(d, terms) {

  l <- lapply(split(d, d$trial), function(t) {
    duration <- cumsum(t$duration)
    duration <- rep(duration, each=2)
    duration <- c(0, duration[-length(duration)])
    data.frame(
      duration = duration,
      x = rep(t$x, each=2),
      i = c(rbind(rep(NA, nrow(t)), t$i)),
      trial = t$trial[1],
      groups = rep(t$groups, each=2))
  })

  d <- do.call(rbind, l)

  if (length(unique(d$groups))==1) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, duration))
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, duration, colour=groups))
  }

  p +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ trial)

}

plot_scanpaths.2d <- function(d, terms) {

  if (length(unique(d$groups))==1) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, size=duration))
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, size=duration, colour=groups))
  }

  p +
    ggplot2::geom_path(size=1) +
    ggplot2::geom_point(alpha=0.2) +
    ggplot2::facet_wrap(~ trial) +
    ggplot2::scale_size_area(name="Duration:")

}

#' This function plots the alignment of fixations in two scanpaths
#'
#' @param s is a data frame containing the first scanpath.  Each line
#'   represents one fixation.  The fixations have to be listed in
#'   chronological order.  Required columns are: x- and y- coordinate
#'   of fixations, and fixation durations.  See the example data set
#'   provided with this package.
#' @param t is the second scanpath.  Same format as for s.
#' @param a is the alignment data frame as returned by
#'   \code{\link{rscasim}}.
#' @param formula specifies which columns in the given data frame are
#'   should be used.  The left-hand side specifies the column that
#'   contains the fixation duration.  The right-hand side consist of
#'   terms that specify x- and y-coordinate of the fixations (in that
#'   order).  See examples below.
#' @param offset_x offset in the x dimension used for scanpath t.  Can
#'   be used to reduce clutter.
#' @param offset_y like \code{offset_x} but in the y dimension.
#' @param nudge_x x offset used for fixation indices.
#' @param nudge_y x offset used for fixation indices.
#' @return A ggplot object.  Can be modified before display, for
#'   example, by adding axis limits, labels, and titles, etc.  See
#'   examples below.
#' @export
#' @examples
#' data(eyemovements)
#' s <- subset(eyemovements, trial==1)
#' t <- subset(eyemovements, trial==9)
#' alignment <- rscasim(s, t, duration ~ x + y | trial,
#'                      512, 384, 60, 1/30)
#' plot_alignment(s, t, alignment, duration ~ x + y | trial, 10, 20)
plot_alignment <- function(s, t, a, formula, offset_x=0, offset_y=0, nudge_x=0, nudge_y=0) {

  s <- prepare.data(s, formula)
  t <- prepare.data(t, formula)

  x <- dplyr::left_join(a, s, by=c("s" = "i"))
  x <- dplyr::left_join(x, t, by=c("t" = "i"), suffix=c(".s", ".t"))
  x <- dplyr::mutate(
                x,
                xmid = ifelse(is.na(x.s), x.t+offset_x,
                              ifelse(is.na(x.t), x.s, (x.s+x.t+offset_x)/2)),
                ymid = ifelse(is.na(y.s), y.t+offset_y,
                              ifelse(is.na(y.t), y.s, (y.s+y.t+offset_y)/2)))
  ggplot2::ggplot(x, ggplot2::aes(x=x.s, y=y.s, xend=x.t+offset_x, yend=y.t+offset_y)) +
    ggplot2::geom_segment(color="red", alpha=0.3, size=0.75) +
    ggplot2::geom_text(ggplot2::aes(x=xmid, y=ymid, label=round(cost)), color="red", size=3, hjust=1.5, vjust=1.5) +
    ggplot2::geom_point(ggplot2::aes(size=duration.s, color=ifelse(is.na(x.t), "red", "black")), alpha=0.2) +
    ggplot2::geom_point(ggplot2::aes(x=x.t+offset_x, y=y.t+offset_y, size=duration.t, color=ifelse(is.na(x.s), "red", "black")), alpha=0.2) +
    ggplot2::geom_path(size=1) +
    ggplot2::geom_path(ggplot2::aes(x=x.t+offset_x, y=y.t+offset_y), size=1) +
    ggplot2::geom_text(ggplot2::aes(label=s), alpha=0.5, hjust=1, vjust=-1) +
    ggplot2::geom_text(ggplot2::aes(x=x.t+offset_x, y=y.t+offset_y, label=t), alpha=0.5, hjust=1, vjust=-1) +
    ggplot2::scale_color_identity(name="Fixation match:", labels=c("match", "no match"), guide="legend") +
    ggplot2::scale_size_area(name="Fixation duration:") +
    ggplot2::labs(x="x (pixels)", y="y (pixels)")

}

