\name{print.mvna}
\alias{print.mvna}

\title{ Print method for 'mvna' object }
\description{
  Print method for an object of class 'mvna'. It prints estimates of
  the cumulative hazard along with estimates of the variance described
  in eq. (4.1.6) and (4.1.7) of Andersen et al. (1993) at several
  timepoints obtained with the 'quantile' function.
}
\usage{
\method{print}{mvna}(x, \dots)
}

\arguments{
  \item{x}{An object of class 'mvna'}
  \item{\dots}{Other arguments for print method }
}

\value{
 No value returned
}
\author{ Arthur Allignol, \email{arthur.allignol@fdm-freiburg.de} }
\seealso{ \code{\link{mvna}} }

\keyword{ print }
