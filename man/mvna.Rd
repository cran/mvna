\name{mvna}
\alias{mvna}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nelson-Aalen estimator in multistate models }
\description{
  This function computes the multivariate Nelson-Aalen estimator of
  the cumulative transition hazards in multistate models, that is,
  for each possible transition, it computes an estimate of
  the cumulative hazard.
}
\usage{
mvna(data, state.numbers, tra, cens.name, tr.choice = "all")
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
  \item{state.numbers}{ A vector of character giving the states names. }
  \item{tra}{A quadratic matrix of logical values describing the possible
    transitions within the multistate model. }
  \item{cens.name}{ A character giving the code for censored
    observations in the column 'to' of \code{data}. If there is no
    censored observations in your data, put 'NA'.}
  \item{tr.choice}{ A vector of character specifying which transitions
    are to be computed, \emph{e.g.} c("0 1","0 2") for transitions from
    state named '0' to state named '1', and from state '0' to state '2'.
    Default is "all".  }
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
  
  \code{na}: Nelson-Aalen estimates at each transition times.
  
  \code{var1}: Variance estimator given in eq. (4.1.6) of Andersen et
  al. (1993).
  
  \code{var2}: Variance estimator given in eq. (4.1.7) of Andersen et
  al. (1993).
  
  \code{time}: The transition times.

  The list also contains:
  \item{state.numbers}{ The same as in the function call.}
  \item{cens.name}{The same as in the function call.}
  \item{n.cens}{Number of censored observations}
  }
\note{
  The variance estimator (4.1.6) may overestimate the true variance, and
  the one defined eq. (4.1.7) may underestimate the true variance (see
  Klein (1991) and Andersen et al. (example IV.1.1, 1993)), especially
  with small sample set. Therefore, a warning will be produced for
  risk-sets smaller than 5. Klein (1991) recommends the use of the variance
  estimator of eq. (4.1.6) because he found it less biased. We suggest
  that bootstrapping this variance estimator may improve on this
  problem (see \code{\link{sir.cont}} for an example of
  bootstrap).
  }
\references{Andersen, P.K., Borgan, O., Gill, R.D. and Keiding,
  N. (1993). \emph{Statistical models based on counting
    processes}. Springer Series in Statistics. New York, NY: Springer.

  Klein, J.P. Small sample moments of some estimators of the variance
  of the Kaplan-Meier and Nelson-Aalen estimators. \emph{Scandinavian
  Journal of Statistics}, 18:333--340, 1991.}

\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{ \code{\link{xyplot.mvna}},\code{\link{sir.adm}},\code{\link{sir.cont}}}
\examples{
data(sir.cont)

# Matrix of logical giving the possible transitions
tra <- matrix(ncol=3,nrow=3,TRUE)
tra[3,1:2] <- FALSE
diag(tra) <- FALSE # doesn't change much

# Computation of the Nelson-Aalen estimates
na <- mvna(sir.cont,c("0","1","2"),tra,"cens")

# plot
xyplot(na)

### See the example in the data sets help pages
### help(sir.adm) and help(sir.cont)
### for more detailed examples
}
  

\keyword{survival}

