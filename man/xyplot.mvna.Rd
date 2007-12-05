\name{xyplot.mvna}
\alias{xyplot.mvna}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Panel plots for object of class 'mvna' }
\description{
  xyplot function for objects of class 'mvna'. Estimates of the
  cumulative hazards are plotted as a function of time for all the
  transitions specified by the user. This function could also plot
  several types of pointwise confidence interval (see Andersen et
  al. (1993) p.208).
}
\usage{
\method{xyplot}{mvna}(x, data = NULL, xlab = "Time",
              ylab = "Cumulative Hazard", tr.choice = "all",
              conf.int = TRUE, ci.type = c(1, 2), level = 0.05,
              col = c(1, 1, 1), lty = c(1, 3, 3), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of class 'mvna'. }
  \item{data}{\emph{Useless}. Just to avoid warnings while
    compiling the package.}
  \item{xlab}{ x-axis label. Default is "Time". }
  \item{ylab}{ y-axis label. Default is "Cumulative Hazard" }
  \item{tr.choice}{A character vector of the form 'c("from to","from
    to")' specifying which transitions should be plotted. Default is "all".  }
  \item{conf.int}{ A logical whether plot pointwise confidence
    interval. Default is TRUE}
  \item{ci.type}{ A vector of 2 integer coding for the confidence
    interval type. The first part of the vector can be 1 or 2, 1 for
    the variance estimator described in eq. (4.1.6) of Andersen et
    al. (1993), 2 for the one described in eq. (4.1.7). The second part
    of the vector can be 1,2 or 3. 1 for a linear confidence interval, 2
  for log-transformation, 3 for arcsin-transformation. Default is c(1,2).}
  \item{level}{ Level of the confidence interval. Default is 0.05. }
  \item{col}{ Vector of colour for the plot. Default is black. }
  \item{lty}{ Vector of line type. Default is c(1,3,3) }
  \item{\dots}{ Other arguments for xyplot}
}

\value{
  An object of class 'trellis'.
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
\seealso{ \code{\link{mvna}}, \code{\link{sir.adm}},\code{\link{sir.cont}}   }

\keyword{ device }
