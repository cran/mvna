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
  \item{id:}{Randomly generated patient id}
  \item{from:}{State from which a transition occurs}
  \item{to:}{State to which a transition occurs}
  \item{time:}{Time when a transition occurs}
  
  The possible states are:
  \item{0:}{No ventilation}
  \item{1:}{Ventilation}
  \item{2:}{End of stay}

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
tra <- matrix(ncol=3,nrow=3,TRUE)
tra[3,1:2] <- FALSE

# Computation of the Nelson-Aalen estimates
  na.cont <- mvna(sir.cont,c("0","1","2"),tra,"cens")

  xyplot(na.cont,tr.choice=c("0 2","1 2"),aspect=1,
       strip=strip.custom(bg="white",
         factor.levels=c("No ventilation -- Discharge/Death",
           "Ventilation -- Discharge/Death"),
         par.strip.text=list(cex=0.9)),
       scales=list(alternating=1),xlab="Days",
       ylab="Nelson-Aalen estimates")

## Not run: 
  # Bootstrap
  # Bootstrap statistics. Works for one transition at a time
  na.boot <- function(data,index,state.names,tra,cens.name,ctr) {
  temp <- mvna(data[index,],state.names,tra,
               cens.name,tr.choice=ctr)[[ctr]]$var1
  return(temp)
  }

  # Matrix of possible transitions
  tra <- matrix(ncol=3,nrow=3,TRUE)
  tra[3,1:2] <- FALSE

  # Test
  na.boot(sir.cont,1:nrow(sir.cont),
          c("0","1","2"),tra,"cens","0 2")

  library(boot)
  nb <- 10 ## nb=1000
  var.boot <- boot(sir.cont,na.boot,nb,
                   state.names=c("0","1","2"),
                   tra=tra,cens.name="cens",ctr="0 2",sim="permutation")

  m <- apply(var.boot$t,MARGIN=2,FUN=mean)
## End(Not run)
}
\keyword{datasets}