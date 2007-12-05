\name{n.events}
\alias{n.events}
\alias{n.events.mvna}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Number of events for event history analysis}
\description{
  This function gives the number of events or transitions and percentage.
}
\usage{
n.events(object,\dots)

\method{n.events}{mvna}(object,data,\dots)
}
\arguments{
  \item{object}{The object on which method dispatch is carried out.

    For \code{"mvna"} methods, object must be of class 'mvna'}
  \item{data}{ A data.frame as in \code{\link{mvna}}, corresponding to
    object}
  \item{\dots}{Further arguments for the \code{n.events} method}
  }
\value{
  For \code{n.events.mvna}, returns a list with components:
  \item{n.trans}{A matrix whose cells give the number of transitions
    from one state to another. The starting state is given by the row
    names, the column names give the arrival state.
  }
  \item{percent}{A matrix whose cells give the transition percentage. To
  be read as \code{n.trans}.}
  \item{censoring}{A vector of 2 elements. The first one gives the
    number of censored observations, the second one its percentage.}
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{ \code{\link{mvna}},\code{\link{sir.adm}},\code{\link{sir.cont}}}
\examples{
data(sir.cont)

# Matrix of logical giving the possible transitions
tra <- matrix(ncol=3,nrow=3,TRUE)
tra[3,1:2] <- FALSE
diag(tra) <- FALSE # doesn't change much

# Computation of the Nelson-Aalen estimates
na <- mvna(sir.cont,c("0","1","2"),tra,"cens")

n.events(na,sir.cont)
}
  

\keyword{misc}
