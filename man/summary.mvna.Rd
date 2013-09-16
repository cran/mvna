\name{summary.mvna}
\Rdversion{1.1}
\alias{summary.mvna}
\alias{print.summary.mvna}

\title{
  Summary method for objects of class 'mvna' 
}
\description{
  Summary method for \code{mvna} objects. The function returns a list
  containing the cumulative transition hazards, variance and other
  informations. 
}
\usage{
\S3method{summary}{mvna}(object, level = 0.95,
        var.type = c("aalen", "greenwood"),
        ci.fun = c("log", "linear", "arcsin"), ...)

\S3method{print.summary}{mvna}(x, ...)
}
\arguments{
  \item{object}{An object of class \code{mvna}.}
  \item{level}{Level of the pointwise confidence interval. Default is
  0.95.}
  \item{var.type}{Which of the "aalen" or "greenwood" variance estimator
  should be displayed and used to compute the pointwise confidence
  intervals. Default is "aalen".}
  \item{ci.fun}{Which transformation to apply to the confidence
    intervals. One of "linear", "log", "arcsin". Default is "log".}
  \item{\dots}{Further arguments.}
  \item{x}{An object of class \code{summary.mvna}.}
}
\value{
  Returns an object of class \code{mvna} which is a list of data frames
  named after the possible transitions. Each data frame contains the
  following columns:
  \item{time}{Event times at which the cumulative hazards are
  estimated.}
  \item{na}{Estimated cumulative transition hazards.}
  \item{var.aalen or var.greenwood}{Variance estimates. The name depends
    on the \code{var.type} argument. Default will be \code{var.aalen}.}
  \item{lower}{Lower bound of the pointwise confidence interval.}
  \item{upper}{Upper bound.}
  \item{n.risk}{Number of individuals at risk of experiencing an event
  just before \eqn{t}{t}.}
  \item{n.event}{Number of transitions at time \eqn{t}{t}.}
}
\author{
  Arthur Allignol, \email{arthur.allignol@uni-ulm.de}
}
\seealso{
  \code{\link{mvna}}
}
\examples{
data(sir.adm)

## data set transformation
data(sir.adm) 
id <- sir.adm$id
from <- sir.adm$pneu
to <- ifelse(sir.adm$status==0,"cens",sir.adm$status+1)
times <- sir.adm$time
dat.sir <- data.frame(id,from,to,time=times)

## Possible transitions
tra <- matrix(ncol=4,nrow=4,FALSE)
tra[1:2,3:4] <- TRUE

na.pneu <- mvna(dat.sir,c("0","1","2","3"),
                tra,"cens")

summ.na.pneu <- summary(na.pneu)

## cumulative hazard for 0 -> 2 transition:
summ.na.pneu$"0 2"$na
}
\keyword{survival}
