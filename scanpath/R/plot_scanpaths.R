

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

# Plot the alignment of two scanpaths:
plot_alignment <- function(s, t, a, offset_x=NULL, offset_y=NULL, nudge_x=10, nudge_y=10) {
  
  s$match <- a[[1]]
  t$match <- a[[2]]
  s$idx <- 1:nrow(s)
  t$idx <- 1:nrow(t)
  if(is.null(offset_x)) offset_x <- (max(s$x) - min(t$x)) * 1.05
  if(is.null(offset_y)) offset_y <- (max(s$y) - min(t$y)) * 1.05
  
  s %>%
    filter(!is.na(match)) %>%
    mutate(
      xend = t$x[match] + offset_x,
      yend = t$y[match] + offset_y,
      xmid = (x + xend) / 2,
      ymid = (y + yend) / 2,
      cost = a[[3]]) %>%
    ggplot(aes(x, y)) +
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend), color="red", alpha=0.3, size=0.75) +
    geom_text(aes(x=xmid, y=ymid, label=round(cost)), color="red", size=3) + 
    geom_point(aes(size=duration, color=ifelse(is.na(match), "red", "black")), s, alpha=0.2) +
    geom_path(data=s, size=1) + 
    geom_text(aes(label=idx), s, nudge_x=nudge_x, nudge_y=nudge_y, alpha=0.5) +
    geom_point(aes(x+offset_x, y+offset_y, size=duration, color=ifelse(is.na(match), "red", "blue")), t, alpha=0.2) +
    geom_path(aes(x+offset_x, y+offset_y), t, color="blue", size=1) +
    geom_text(aes(x+offset_x, y+offset_y, label=idx), t, nudge_x=nudge_x, nudge_y=nudge_y, color="blue", alpha=0.5) +
    labs(x="x (pixels)", y="y (pixels)") +
    scale_size_area(name="Fixation duration:", breaks=c(100, 200, 300), labels=scales::unit_format("ms")) +
    scale_color_identity(name="Fixation match:", labels=c("Scanpath 1 match", "Scanpath 2 match", "No match"), guide="legend")
  
}

