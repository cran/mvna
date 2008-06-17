plot.mvna <- function(x, tr.choice, xlab="Time",
                      ylab="Nelson-Aalen estimates",
                      legend=TRUE, curvlab, locator=FALSE,
                      coord, col, lty, xlim, ylim, ...) {
  if (!inherits(x, "mvna"))
    stop("Argument 'x' must be of class 'mvna'")
  if (missing(tr.choice))
    tr.choice <- names(x)[1:(length(x) - 7)]
  else {
    if (!all(tr.choice %in% names(x))) {
      stop("Argument 'tr.choice' and the possible transitions must match")
    }
  }
  x <- x[tr.choice]
  lt <- length(x)
  if (missing(col)) {
    col <- rep(1, lt)
  }
  if (missing(lty)) {
    lty <- 1:lt
  }
  if (length(col) < lt)
    col <- col * rep(1, lt)
  if (length(lty) < lt)
    lty <- lty * rep(1, lt)
  if (missing(xlim)) {
    lim <- sapply(1:lt, function(i) {
      max(x[[i]]$time, na.rm=TRUE)
    })
    xlim <- c(0, max(lim, na.rm=TRUE))
  }
  if (missing(ylim)) {
    lim <- sapply(1:lt, function(i) {
      max(x[[i]]$na, na.rm=TRUE)
    })
    ylim <- c(0, max(lim, na.rm=TRUE))
  }
  plot(x[[1]]$time, x[[1]]$na, type="s", col=col[1],
       lty=lty[1], xlim=xlim, ylim=ylim, xlab=xlab,
       ylab=ylab, ...)
  if (lt > 1) {
    for (i in 1:lt) {
      lines(x[[i]]$time, x[[i]]$na, col=col[i], lty=lty[i],
            type="s", ...)
    }
  }
  if (legend) {
    if (missing(curvlab)) {
      curvlab <- names(x)
    }
    if (locator) {
      legend(locator(1), curvlab, col=col, lty=lty,
             ...)
    }
    else {
      if (missing(coord)) {
        coord <- c(xlim[1]+0.3,ylim[2]-0.03)
      }
      legend(coord[1], coord[2], curvlab, col=col,
             lty=lty, ...)
    }
  }
}
