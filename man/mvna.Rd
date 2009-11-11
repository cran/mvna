\name{mvna}
\alias{mvna}

\title{Nelson-Aalen estimator in multistate models }
\description{
  This function computes the multivariate Nelson-Aalen estimator of
  the cumulative transition hazards in multistate models, that is,
  for each possible transition, it computes an estimate of
  the cumulative hazard.
}

\usage{
mvna(data, state.names, tra, cens.name)
}
\arguments{
  \item{data}{ A data.frame of the form data.frame(id,from,to,time)
    or (id,from,to,entry,exit)
    \describe{
    \item{id:}{patient id}
    \item{from:}{the state from where the transition occurs}
    \item{to:}{the state to which a transition occurs}
    \item{time:}{time when a transition occurs}
    \item{entry:}{entry time in a state}
    \item{exit:}{exit time from a state}
    }
  This data.frame is transition-oriented, \emph{i.e.} it contains one
  row per transition, and possibly several rows per patient. Specifying
  an entry and exit time permits to take into account left-truncation.}
  \item{state.names}{ A vector of character giving the states names. }
  \item{tra}{A quadratic matrix of logical values describing the possible
    transitions within the multistate model. }
  \item{cens.name}{ A character giving the code for censored
    observations in the column \code{to} of \code{data}. If there is no
    censored observations in your data, put \code{NULL}.}
}

\details{
  This functions computes the Nelson-Aalen estimator as described in
  Anderson et al. (1993), along with the two variance estimators
  described in eq. (4.1.6) and (4.1.7) of Andersen et al. (1993) at each
  transition time.
}

\value{
  Returns a list named after the possible transitions, \emph{e.g.} if we
  define a multistate model with two possible transitions: from state 0 to
  state 1, and from state 0 to state 2, the returned list will have two
  parts named "0 1" and "0 2". Each part contains a data.frame with
  columns:
  \item{na}{Nelson-Aalen estimates at each transition times.}
  \item{var.aalen}{Variance estimator given in eq. (4.1.6) of Andersen et
    al. (1993).}
  \item{var.greenwood}{Variance estimator given in eq. (4.1.7) of Andersen et
    al. (1993).}
  \item{time}{The transition times.}
  The list also contains:
  \item{time}{All the event times.}
  \item{n.risk}{A matrix giving the number at individual at risk in the
    transient states just before an event.}
  \item{n.event}{An array which gives the number of transitions at each
    event times.}
  \item{n.cens}{A matrix giving the number a censored observations at each
  event times.}
  \item{state.names}{ The same as in the function call.}
  \item{cens.name}{The same as in the function call.}
  \item{trans}{A data frame, with columns \code{from} and \code{to}, that gives
  the possible transitions.}
}
\note{
  The variance estimator (4.1.6) may overestimate the true variance, and
  the one defined eq. (4.1.7) may underestimate the true variance (see
  Klein (1991) and Andersen et al. (example IV.1.1, 1993)), especially
  with small sample set. Klein (1991) recommends the use of the variance
  estimator of eq. (4.1.6, "aalen") because he found it to be less biased.
  }
\references{Andersen, P.K., Borgan, O., Gill, R.D. and Keiding,
  N. (1993). \emph{Statistical models based on counting
    processes}. Springer Series in Statistics. New York, NY: Springer.

  Klein, J.P. Small sample moments of some estimators of the variance
  of the Kaplan-Meier and Nelson-Aalen estimators. \emph{Scandinavian
  Journal of Statistics}, 18:333--340, 1991.}

\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link{sir.adm}},\code{\link{sir.cont}}}
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

# plot
xyplot(na)

### example with left-truncation
data(abortion)

# Data set modification in order to be used by mvna
names(abortion) <- c("id", "entry", "exit", "from", "to")
abortion$to <- abortion$to + 1

## computation of the matrix giving the possible transitions
tra <- matrix(FALSE, nrow = 5, ncol = 5)
tra[1:2, 3:5] <- TRUE

na.abortion <- mvna(abortion, as.character(0:4), tra, NULL)

plot(na.abortion, tr.choice = c("0 4", "1 4"),
     curvlab = c("Control", "Exposed"),
     bty = "n", legend.pos = "topleft")
}
  

\keyword{survival}