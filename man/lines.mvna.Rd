\name{lines.mvna}
\Rdversion{1.1}
\alias{lines.mvna}
\title{
  Lines method for 'mvna' objects
}
\description{
  Lines method for \code{mvna} objects.
}
\usage{
\S3method{lines}{mvna}(x, tr.choice, col = 1, lty, conf.int = FALSE,
      level = 0.95, var.type = c("aalen", "greenwood"),
      ci.fun = c("log", "linear", "arcsin"), ci.col = col,
      ci.lty = 3, ...)
}
\arguments{
  \item{x}{An object of class \code{mvna}.}
  \item{tr.choice}{A character vector of the form \code{c("from to",
      "from to")} specifying which transitions should be displayed.
  By default, all the transition hazards are plotted.}
  \item{col}{A vector of colours. Default is black.}
  \item{lty}{A vector of line types. Default is 1:number of
    transitions.}
  \item{conf.int}{Logical. Indicates whether to display pointwise
    confidence interval. Default is \code{FALSE}.}
  \item{level}{Level of the confidence interval. Default is 0.95.}
  \item{var.type}{Specifies the variance estimator that should be used
    to compute the confidence interval. One of "aalen" or
    "greenwood". Default is "aalen".}
  \item{ci.fun}{Specifies the transformation applied to the confidence
    interval. Choices are "linear", "log", "arcsin". Default is "log".}
  \item{ci.col}{Colours of the confidence interval lines. By default,
    \code{ci.col} equals \code{col}.}
  \item{ci.lty}{Line types for the confidence intervals. Default is 3.}
  \item{\dots}{Further arguments for \code{lines}.}
}
\value{
  No value returned.
}
\author{
  Arthur Allignol, \email{arthur.allignol@uni-ulm.de}
}
\seealso{
  \code{\link{mvna}}, \code{\link{plot.mvna}}
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

plot(na.pneu, tr.choice = c("0 2"), conf.int = TRUE,
     col = 1, lty = 1, legend = FALSE)

lines(na.pneu, tr.choice = c("1 2"), conf.int = TRUE,
      col = 2, lty = 1)
}
\keyword{aplot}
\keyword{survival}