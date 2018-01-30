

#' This function plots a set of scanpaths, each scanpath in a separate
#' panel.
#'
#' @param formula specifies the format of the plots.  For example,
#'   \code{dur ~ word | trial} puts the words on the x-axis and time
#'   on the y-axis.  Each trial is plotted in its own panel.  This
#'   assumes that words are numbered in ascending order.  Another
#'   example: \code{dur ~ x + y | trial} plots a circle for each
#'   fixation.  The location of the circle is determined by the x- and
#'   y-coordinates of the fixation and its size by the
#'   duration.  Other variables of interest can be used instead of
#'   duration, for example pupil size.  Note that the left-hand side
#'   variable is log-transformed in circle plots.
#' @param d is the data frame containing the variables used in the
#'   formula.
#' @param groups a variable used to distinguish different groups of
#'   scanpaths by color.  Can be used to color-code conditions or
#'   participants.
#' @return A ggplot object.  Can be modified before display, for
#'   example, by adding axis limits etc.  See examples below.
#' @export
#' @examples
#' data(eyemovements)
#' plot_scanpaths(duration ~ word | trial, eyemovements)
#' plot_scanpaths(duration ~ word | trial, eyemovements, subject)
#' plot_scanpaths(duration ~ x + y | trial, eyemovements)
#' plot_scanpaths(duration ~ x + y | trial, eyemovements, subject)
#' p <- plot_scanpaths(duration ~ x + y | trial, eyemovements, subject)
#' p + ggplot2::xlim(0, 600) + ggplot2::ylim(284, 484)
plot_scanpaths <- function(formula, d, groups=NULL) {

  groups <- eval(substitute(groups), d, parent.frame())
  d <- prepare.data(d, formula)
  if (is.null(groups)) {
    d$groups <- 1
  } else {
    d$groups <- groups
  }

  terms <- strsplit(deparse(formula), " [~+|] ")[[1]]

  if (length(terms)==3)
    p <- plot_scanpaths.1d(d, terms) + ggplot2::labs(y="Time")
  else if (length(terms)==4)
    p <- plot_scanpaths.2d(d, terms)

  p +
    ggplot2::theme(legend.position = "top") +
    ggplot2::scale_colour_discrete(name = "Group:") +
    ggplot2::scale_size(name = "Duration (log10):")
}

plot_scanpaths.1d <- function(d, terms) {

  l <- lapply(split(d, d$trial), function(t) {
    duration <- cumsum(t$duration)
    duration <- rep(duration, each=2)
    duration <- c(0, duration[-length(duration)])
    data.frame(
      duration = duration,
      x = rep(t$x, each=2),
      trial = t$trial[1],
      group = rep(t$groups, each=2))
  })

  d <- do.call(rbind, l)

  if (length(unique(d$group))==1) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, duration))
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, duration, colour=group))
  }
  
  p +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~ trial)

}

plot_scanpaths.2d <- function(d, terms) {

  if (length(unique(d$group))==1) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, size=log10(duration)))
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, size=log10(duration), colour=groups))
  }
  
  p +
    ggplot2::geom_path(size=1) +
    ggplot2::geom_point(alpha=0.2) +
    ggplot2::facet_wrap(~ trial)

}
