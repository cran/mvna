plot.mvna <- function(x, tr.choice, xlab="Time",
                      ylab="Nelson-Aalen estimates",
                      col, lty, xlim, ylim, legend = TRUE,
                      legend.pos, curvlab, legend.bty = "n", ...) {
    if (!inherits(x, "mvna"))
        stop("Argument 'x' must be of class 'mvna'")
    nt <- nrow(x$trans)
    if (missing(tr.choice))
        tr.choice <- names(x)[1:nt]
    else {
        if (!all(tr.choice %in% names(x[1:nt]))) {
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
    plot(x$time, x$time, xlim = xlim, ylim = ylim,
         type = "n", xlab = xlab, ylab = ylab, ...)
    for (i in seq_along(tr.choice)) {
        lines(x[[i]]$time, x[[i]]$na, col=col[i], lty=lty[i],
            type="s", ...)
    }
    if (legend) {
        if (missing(legend.pos))
            legend.pos <- "topright"
        if (missing(curvlab))
            curvlab <- tr.choice
        if (is.list(legend.pos)) legend.pos <- unlist(legend.pos)
        if (length(legend.pos) == 1) {
            xx <- legend.pos
            yy <- NULL
        }
        if (length(legend.pos) == 2) {
            xx <- legend.pos[1]
            yy <- legend.pos[2]
        }
        args <- list(...)
        ii <- pmatch(names(args),
                     names(formals("legend")[-charmatch("bty",names(formals("legend")))]))
        do.call("legend", c(list(xx, yy, curvlab, col=col, lty=lty, bty = legend.bty),
                            args[!is.na(ii)]))
    }
    invisible(x)
}
