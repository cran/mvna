\name{plot.mvna}
\alias{plot.mvna}

\title{Plot method for a mvna object}
\description{
  Plot method for an object of class \code{mvna}. This function plots
  estimates of the cumulative transition hazards in one panel. 
}
\usage{
\S3method{plot}{mvna}(x, tr.choice, xlab = "Time",
     ylab = "Cumulative Hazard", col = 1, lty, xlim, ylim,
     conf.int = FALSE, level = 0.95,
     var.type = c("aalen", "greenwood"),
     ci.fun = c("log", "linear", "arcsin"),
     ci.col = col, ci.lty = 3,
     legend = TRUE, legend.pos, curvlab, legend.bty = "n", ...)
}
\arguments{
  \item{x}{An object of class \code{mvna}.}
  \item{tr.choice}{A character vector of the form \code{c("from to","from
    to")} specifying which transitions should be plotted. Default, all
    the cumulative transition hazards are plotted.}
  \item{xlab}{x-axis label. Default is "Time".}
  \item{ylab}{y-axis label. Default is "Cumulative Hazard".}
  \item{col}{Vector of colour. Default is black.}
  \item{lty}{Vector of line type. Default is 1:number of transitions}
  \item{xlim}{Limits of x-axis for the plot}
  \item{ylim}{Limits of y-axis for the plot}
  \item{conf.int}{Logical. Whether to display pointwise confidence
    intervals. Default is FALSE.}
  \item{level}{Level of the pointwise confidence intervals. Default is
    0.95.}
  \item{var.type}{A character vector specifying the variance that
    should be used to compute the pointwise confidence
    intervals. Choices are "aalen" or "greenwood". Default is "aalen".}
  \item{ci.fun}{One of "log", "linear" or "arcsin". Indicates which
    transformation to apply to the confidence intervals.}
  \item{ci.col}{Colour for the confidence intervals. By default, the
    colour specified by \code{col} is used.}
  \item{ci.lty}{Line type for the confidence intervals. Default is 3.}
  \item{legend}{A logical specifying if a legend should be added}
  \item{legend.pos}{A vector giving the legend's position. See
    \code{\link{legend}} for further details.}
  \item{curvlab}{A character or expression vector to appear in the
    legend. Default is the name of the transitions.}
  \item{legend.bty}{Box type for the legend.}
  \item{\dots}{Further arguments for plot method.}
}
\details{
  This plot method permits to draw several cumulative transition hazards
  on the same panel.
}
\value{
  No value returned
}

\author{Arthur Allignol \email{arthur.allignol@uni-ulm.de}}
\seealso{\code{\link{mvna}}}
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

tra <- matrix(ncol=3,nrow=3,FALSE)
tra[1, 2:3] <- TRUE
tra[2, c(1, 3)] <- TRUE

na.cont <- mvna(sir.cont,c("0","1","2"),tra,"cens")

plot(na.cont, tr.choice=c("0 2", "1 2"))
}
\keyword{hplot}
\keyword{survival}