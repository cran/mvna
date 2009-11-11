\name{xyplot.mvna}
\alias{xyplot.mvna}

\title{ Panel plots for object of class 'mvna' }
\description{
  xyplot function for objects of class \code{mvna}. Estimates of the
  cumulative hazards are plotted as a function of time for all the
  transitions specified by the user. The function can also plot
  several types of pointwise confidence interval (see Andersen et
  al. (1993) p.208).
}
\usage{
\method{xyplot}{mvna}(x, data = NULL, xlab = "Time",
       ylab = "Cumulative Hazard", tr.choice = "all",
       conf.int = TRUE, var.type = c("aalen", "greenwood"),
       ci.fun = c("log", "linear", "arcsin"), level = 0.95,
       col = c(1, 1, 1), lty = c(1, 3, 3),
       ci.type = c(1, 2), ...)
}

\arguments{
  \item{x}{An object of class \code{mvna}.}
  \item{data}{\emph{Useless}.}
  \item{xlab}{ x-axis label. Default is "Time". }
  \item{ylab}{ y-axis label. Default is "Cumulative Hazard" }
  \item{tr.choice}{A character vector of the form \code{c("from to","from
    to")} specifying which transitions should be plotted. Default is "all".  }
  \item{conf.int}{ A logical whether plot pointwise confidence
    interval. Default is TRUE}
  \item{var.type}{One of "aalen" or "greenwood". Specifies which
    variance estimator is used to compute the confidence intervals.}
  \item{ci.fun}{One of "log", "linear" or "arcsin". Indicates the
  transformation applied to the pointwise confidence intervals. Default
  is "log".}
  \item{level}{Level of the confidence interval. Default is 0.95.}
  \item{col}{Vector of colour for the plot. Default is black.}
  \item{lty}{Vector of line type. Default is \code{c(1, 3, 3)}.}
  \item{ci.type}{\emph{DEPRECATED}}
  \item{\dots}{Other arguments for xyplot.}
}

\value{
  An object of class \code{trellis}.
}

\note{
  These plots are highly customizable, see
  \code{\link[lattice]{Lattice}} and \code{\link[lattice]{xyplot}}. For
  example, if one want to change strip background color and the title of
  each strip, it can be added 'strip=strip.custom(bg="a
  color",factor.levels="a title","another title")'. One can use
  'aspect="1"' to get the size of the panels isometric.
  }

\references{ Andersen, P.K., Borgan, O., Gill, R.D. and Keiding,
  N. (1993). \emph{Statistical models based on counting
    processes}. Springer Series in Statistics. New York, NY: Springer.
  
  Deepayan Sarkar (2006). lattice: Lattice Graphics. R package version
    0.13-8.}
\author{Arthur Allignol, \email{arthur.allignol@fdm-freiburg.de} }
\seealso{\code{\link[lattice]{xyplot}}, \code{\link{mvna}}, \code{\link{sir.adm}},\code{\link{sir.cont}}   }

\keyword{hplot}
\keyword{survival}
