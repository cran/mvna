\name{sir.cont}
\docType{data}
\alias{sir.cont}
\title{Ventilation status in intensive care unit patients}
\description{
  Time-dependent ventilation status for intensive care unit (ICU)
  patients, a random sample from the SIR-3 study.
}
\usage{
data(sir.cont)
}
\format{
  A data frame with 1161 rows and 4 columns:
  \describe{
  \item{id:}{Randomly generated patient id}
  \item{from:}{State from which a transition occurs}
  \item{to:}{State to which a transition occurs}
  \item{time:}{Time when a transition occurs}
  }

  The possible states are:
  
  0: No ventilation
  
  1: Ventilation
  
  2: End of stay.

  And \code{cens} stands for censored observations.
}

\details{
  This data frame consists in a random sample of the SIR-3 cohort
  data. It focuses on the effect of ventilation on the length of stay
  (combined endpoint discharge/death). Ventilation status is considered
  as a transcient state in an illness-death model. 

  The data frame is directly formated to be used with the \code{mvna}
  function, i.e. it is transition-oriented with one row per transition.
}

\source{
  Beyersmann, J., Gastmeier, P., Grundmann, H.,
  Baerwolff, S., Geffers, C., Behnke, M.,
  Rueden, H., and Schumacher, M. Use of multistate
  models to assess prolongation of intensive care unit stay due to
  nosocomial infection. \emph{Infection Control and Hospital
  Epidemiology}, 27:493-499, 2006.
}

\examples{
data(sir.cont)

# Matrix of possible transitions
tra <- matrix(ncol=3,nrow=3,FALSE)
tra[1, 2:3] <- TRUE
tra[2, c(1, 3)] <- TRUE

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

# Computation of the Nelson-Aalen estimates
  na.cont <- mvna(sir.cont,c("0","1","2"),tra,"cens")

  xyplot(na.cont,tr.choice=c("0 2","1 2"),aspect=1,
       strip=strip.custom(bg="white",
         factor.levels=c("No ventilation -- Discharge/Death",
           "Ventilation -- Discharge/Death"),
         par.strip.text=list(cex=0.9)),
       scales=list(alternating=1),xlab="Days",
       ylab="Nelson-Aalen estimates")

}
\keyword{datasets}