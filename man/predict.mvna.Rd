\name{predict.mvna}
\alias{predict.mvna}
\title{ Calculates Nelson-Aalen estimates at specified time-points }
\description{
  This function gives the Nelson-Aalen estimates at time-points
  specified by the user.
}
\usage{
\S3method{predict}{mvna}(object, times, tr.choice, level = 0.95,
        var.type = c("aalen", "greenwood"),
        ci.fun = c("log", "linear", "arcsin"), ...)
}

\arguments{
  \item{object}{An object of class \code{mvna}}
  \item{times}{Time-points at which one wants the estimates}
  \item{tr.choice}{A vector of character giving for which transitions
    one wants estimates. By default, the function will give the
    Nelson-Aalen estimates for all transitions.}
  \item{level}{Level of the pointwise confidence intervals. Default is
    0.95.}
  \item{var.type}{Variance estimator displayed and used to compute the
    pointwise confidence intervals. One of "aalen" or "greenwood". Default
    is "aalen".}
  \item{ci.fun}{Which transformation to apply for the confidence
  intervals. Choices are "linear", "log" or "arcsin". Default is "log".}
  \item{\ldots}{Other arguments to predict}
}
\value{
   Returns a list named after the possible transitions, \emph{e.g.} if we
  define a multistate model with two possible transitions: from state 0 to
  state 1, and from state 0 to state 2, the returned list will have two
  parts named "0 1" and "0 2". Each part contains a data.frame with
  columns:
  \item{times}{Time points specified by the user.}
  \item{na}{Nelson-Aalen estimates at the specified times.}
  \item{var.aalen or var.greenwood}{Depending on what was specified in
    \code{var.type}.}
  \item{lower}{Lower bound of the pointwise confidence intervals.}
  \item{upper}{Upper bound.}
}
\references{ Andersen, P.K., Borgan, O., Gill, R.D. and Keiding,
  N. (1993). \emph{Statistical models based on counting
    processes}. Springer Series in Statistics. New York, NY: Springer.} 
\author{ Arthur Allignol, \email{arthur.allignol@gmail.com} }
\seealso{\code{\link{mvna}}, \code{\link{summary.mvna}}}
\examples{
data(sir.cont)

# Modification for patients entering and leaving a state
# at the same date
sir.cont <- sir.cont[order(sir.cont$id, sir.cont$time), ]
for (i in 2:nrow(sir.cont)) {
  if (sir.cont$id[i]==sir.cont$id[i-1]) {
    if (sir.cont$time[i]==sir.cont$time[i-1]) {
      sir.cont$time[i-1] <- sir.cont$time[i-1] - 0.5
    }
  }
}

# Matrix of logical giving the possible transitions
tra <- matrix(ncol=3,nrow=3,FALSE)
tra[1, 2:3] <- TRUE
tra[2, c(1, 3)] <- TRUE

# Computation of the Nelson-Aalen estimates
na <- mvna(sir.cont,c("0","1","2"),tra,"cens")

# Using predict
predict(na,times=c(1,5,10,15))
}
\keyword{survival}