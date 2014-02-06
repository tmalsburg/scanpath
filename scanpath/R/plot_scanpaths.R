
#
#  Functions for plotting scanpaths:
#

plot.scanpaths <- function(formula, data, groups=NULL, panel=panel.scanpath,
                           auto.key=if(!is.null(groups)) list(columns = 2, lines=TRUE),
                           ...)
{

  groups <- eval(substitute(groups), data, parent.frame())
  data <- prepare.data(data, formula)

  args <- list(...)
  terms <- strsplit(deparse(formula), " [~+|] ")[[1]]
  xlab <- terms[[2]]

  if (length(data) == 3) {
    colnames(data)[[2]] <- "x"
    plot.func <- plot.scanpaths.1d
    ylab <- "time"
  } else {
    plot.func <- plot.scanpaths.2d
    ylab <- terms[[3]]
  } 

  if (!"xlab" %in% names(args))
    args$xlab <- xlab
  if (!"ylab" %in% names(args))
    args$ylab <- ylab
  
  do.call(plot.func, c(list(data, "groups"=groups, "panel"=panel,
                            auto.key=auto.key), args))
}

# TODO: Fix plots when ROIs are factor (currently it drops unused rois and puts
# wrong labels on the x-axis).
panel.scanpath <- function(x, y, groups=NULL, subscripts=NULL, ...) {

  if (!is.null(groups) & !is.null(subscripts)) {
    group <- as.integer(groups[subscripts][1])
    colors <- trellis.par.get("superpose.line")$col
    col <- colors[[((group-1)%%length(colors))+1]]
    # This is for marking fixations but there's no suitable pch:
    #panel.points(x, y, pch=3, col=col, cex=1)
    panel.lines(x, y, col=col, ...)
  } else {
    #panel.points(x, y, pch=3, cex=1)
    panel.lines(x, y, ...)
  }
}

plot.scanpaths.2d <- function(data, groups=NULL, panel=panel.scanpath,
                              xlim=c(min(data$x), max(data$x)),
                              ylim=c(min(data$y), max(data$y)),
                              ...)
{
  xyplot(y~x|trial, data, panel=panel, xlim=xlim, ylim=ylim,
         groups=groups, drop.unused.levels=list(cond=TRUE, data=FALSE), ...)
}

plot.scanpaths.1d <- function(data, groups=NULL, panel=panel.scanpath,
                              ylim=NULL, ...)
{

  # Create timeline y-axis from fixation durations:

  #l <- levels(data$trial)
  trial <- as.factor(rep(data$trial, each=2))
  #levels(trial) <- l

  if (!is.null(groups)) 
    groups <- rep(groups, each=2)
  x <- rep(data$x, each=2)
  d <- rep(data$d, each=2)
  s <- c(TRUE, diff(as.integer(as.factor(trial))) != 0)
  d <- ifelse(s, 0, d)
  idxs <- setdiff(2:nrow(data)*2, which(s)+1)

  # CHECK: Is it possible to use filter or something similar here?
  for (i in idxs) {
    d[[i-1]] <- d[[i-2]]
    d[[i]] <- d[[i]] + d[[i-1]]
  }

  if (is.null(ylim))
    ylim <- c(min(d), max(d))

  xyplot(d~x|trial, data.frame(x=x, d=d, trial=trial),
         panel=panel, ylim=ylim, groups=groups, drop.unused.levels=list(cond=TRUE, data=FALSE), ...)
}

