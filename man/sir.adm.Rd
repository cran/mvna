\name{sit.adm}
\docType{data}
\alias{sir.adm}
\title{Pneumonia on admission in intenive care unit patients}
\description{
  Pneumonia status on admission for intensive care unit (ICU) patients,
  a random sample from the SIR-3 study.
}
\usage{data(sir.adm)}
\format{
  The data contains 765 rows and 4 variables:
  \item{id:}{Randomly generated patient id}
  \item{pneu:}{Pneumonia indicator. 0: No pneumonia, 1: Pneumonia}
  \item{status}{Status indicator. 0: censored observation, 1:
    discharged, 2: dead}
  \item{time:}{Follow-up time in day}
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
# data set transformation
data(sir.adm) 
id <- sir.adm$id
from <- sir.adm$pneu
to <- ifelse(sir.adm$status==0,"cens",sir.adm$status+1)
times <- sir.adm$time
dat.sir <- data.frame(id,from,to,time=times)

# Possible transitions
tra <- matrix(ncol=4,nrow=4,FALSE)
diag(tra) <- TRUE
tra[1:2,3:4] <- TRUE

na.pneu <- mvna(dat.sir,c("0","1","2","3"),
                tra,"cens")
xyplot(na.pneu,tr.choice=c("0 2","1 2","0 3","1 3"),
       aspect=1,strip=strip.custom(bg="white",
       factor.levels=c("No pneumonia on admission -- Discharge",
                       "Pneumonia on admission -- Discharge",
                       "No pneumonia on admission -- Death",
                       "Pneumonia on admission -- Death"),
       par.strip.text=list(cex=0.9)),
       scales=list(alternating=1),xlab="Days",
       ylab="Nelson-Aalen esimates")
}
\keyword{datasets}