\name{predict.mvna}
\alias{predict.mvna}
\title{ Calculates Nelson-Aalen estimates at specified time-points }
\description{
  This function gives the Nelson-Aalen estimates at time-points
  specified by the user, along with the two variance estimators.
}
\usage{
\method{predict}{mvna}(object, times, tr.choice, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ An object of class 'mvna' }
  \item{times}{ Time-points at which you want the estimates }
  \item{tr.choice}{ A vector of character giving for which transitions
    you want estimates. By default, the function will give the
    Nelson-Aalen estimates for all the transitions.}
  \item{\ldots}{Other arguments to predict}
}
\value{
   Returns a list named after the possible transitions, \emph{e.g.} if we
  define a multistate model with two possible transitions: from state 0 to
  state 1, and from state 0 to state 2, the returned list will have two
  parts named "0 1" and "0 2". Each part contains a data.frame with columns:
  \item{na}{Nelson-Aalen estimates at each transition times.}
  \item{var1}{Variance estimator given in eq. (4.1.6) of Andersen et
    al. (1993).}
  \item{var2}{Variance estimator given in eq. (4.1.7) of Andersen et
    al. (1993).}
  \item{time}{The given timepoints.}
}
\references{ Andersen, P.K., Borgan, O., Gill, R.D. and Keiding,
  N. (1993). \emph{Statistical models based on counting
    processes}. Springer Series in Statistics. New York, NY: Springer.} 
\author{ Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de} }
\seealso{ \code{\link{mvna}}, \code{\link{sir.adm}}, \code{\link{sir.cont}} }
\examples{
data(sir.cont)

# Matrix of logical giving the possible transitions
tra <- matrix(ncol=3,nrow=3,TRUE)
tra[3,1:2] <- FALSE

# Computation of the Nelson-Aalen estimates
na <- mvna(sir.cont,c("0","1","2"),tra,"cens")

# Using predict
predict(na,times=c(1,5,10,15))
}
\keyword{misc}