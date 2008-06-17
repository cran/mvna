\name{plot.mvna}
\alias{plot.mvna}

\title{Plot method for a mvna object}
\description{
  plot method for an object of class 'mvna'. This function plots
  estimates of the cumulative transition hazards in one panel.
}
\usage{
\method{plot}{mvna}(x, tr.choice, xlab = "Time",
ylab = "Nelson-Aalen estimates", legend = TRUE, curvlab, locator =
FALSE, coord, col, lty,
xlim, ylim, ...)
}

\arguments{
  \item{x}{An object of class 'mvna'}
  \item{tr.choice}{A character vector of the form 'c("from to","from
    to")' specifying which transitions should be plotted. Default, all
    the cumulative transition hazards are plotted}
  \item{xlab}{x-axis label. Default is "Time"}
  \item{ylab}{y-axis label. Default is "Nelson-Aalen estimates"}
  \item{legend}{A logical specifying if a legend should be added}
  \item{curvlab}{If legeng=TRUE, a character or expression vector to
    appear in the legend. Default is the name of the transitions}
  \item{locator}{A logical indicating whether use \code{locator} to put
    the legend}
  \item{coord}{If locator=FALSE, a vector of length 2 indicating the
    coordinates of the legend}
  \item{col}{Vector of colour. Default is black}
  \item{lty}{Vector of line type. Default is 1:number of transitions}
  \item{xlim}{Limits of x-axis for the plot}
  \item{ylim}{Limits of y-axis for the plot}
  \item{\dots}{Additional arguments for plot}
}
\details{
  This plot method permits to draw several cumulative transition hazards
  on the same panel.
}
\value{
  No value returned
}

\author{Arthur Allignol \email{arthur.allignol@fdm.uni-freiburg.de}}
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
\keyword{device}