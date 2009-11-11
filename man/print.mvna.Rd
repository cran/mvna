\name{print.mvna}
\alias{print.mvna}

\title{ Print method for 'mvna' object }
\description{
  Print method for an object of class \code{mvna}. It prints estimates of
  the cumulative hazard along with estimates of the variance described
  in eq. (4.1.6) and (4.1.7) of Andersen et al. (1993) at several
  time points obtained with the \code{quantile} function.
}
\usage{
\method{print}{mvna}(x, \dots)
}

\arguments{
  \item{x}{An object of class \code{mvna}}
  \item{\dots}{Other arguments for print method}
}

\value{
  No value returned.
}
\author{ Arthur Allignol, \email{arthur.allignol@fdm-freiburg.de} }
\seealso{\code{\link{mvna}}}

\keyword{print}
\keyword{survival}