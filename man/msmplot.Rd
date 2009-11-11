\name{msmplot}
\alias{msmplot}
\alias{msmplot.mvna}

\title{Combined plot of the multistate model and the cumulative hazards.}
\description{
  This function uses the package \pkg{Rgraphviz} to display the
  multistate model. Next \pkg{GrisBase} is used to put the cumulative
  hazard plots near the arrows representing transitions.
}
\usage{
\S3method{msmplot}{mvna}(object, tr.choice, graph, layout = "dot",
        recip.edges = "combined", order = "LR", unit = "npc",
        width, height, just = "center",
        mgp = c(1.3, 0.4, 0), cex = 0.6,
        mtext = TRUE, label.plots, side = 3, lcex = 0.9,
        conf.int = TRUE, level = 0.05, xlab = "Time", ylab = "",
        ...)
}

\arguments{
  \item{object}{An object of class \code{mvna}.}
  \item{tr.choice}{A character vector of the form ("from to", "from to")
    describing the transitions to plot. By default, the estimated
    cumulative hazards are displayed for all transitions.}
  \item{graph}{Optional. An object of class
    \code{\link[Rgraphviz]{Ragraph}}. People familiar with the
  \pkg{Rgraphviz} and/or willing to have a better control on how the
  multistate model is displayed can provide here their own
  \code{Ragraph} object. If not provided, the multistate plot is done
  internally.}
  \item{layout}{If \code{graph} is not provided, permits to specify the
  layout of the multistate plot.}
  \item{recip.edges}{If \code{graph} is not provided, permits to specify
  how to handle reciprocated edges, defaults is 'combined'.}
  \item{order}{Order for the display of the nodes. Default is 'LR', that
    is from left to right.}
  \item{unit}{A string specifying the units used for creating the
  viewports in which the cumulative hazards will be plotted. Default is
  "npc". See the \pkg{grid} vignette for the possible values.}
  \item{width}{A vector giving the width of the viewport for plotting the
    cumulative hazards.}
  \item{height}{A vector giving the height of the viewport.} 
  \item{just}{A string or numeric vector indicating the justification of
  the viewport.}
  \item{mgp}{Margin line for the axis title, axis labels and axis line
  in the cumulative hazards plots. Default is c(1.3, 0.4, 0), smaller
  that the default value in \code{\link[graphics]{par}} due to the fact
  the plots will usually be small.}
  \item{cex}{A numerical value giving the amount by which plotting text
  and symbols should be magnified relative to the default.}
  \item{mtext}{A logical indicating whether to display a label above the plot.}
  \item{label.plots}{If \code{mtext = TRUE}, specifies the
  labels. Default is the transition numbers.}
  \item{side}{Gives the side where to put the label (1=bottom, 2=left,
  3=top, 4=right). Default is 3.}
  \item{lcex}{Character expansion factor. This is an absolute
  value. Therefore, this value will be evaluate with respect with
  \code{cex} above.} 
  \item{conf.int}{A logical indicating whether plot confidence
  intervals. Default is TRUE}
  \item{level}{Level for the confidence interval. Default is 0.05} 
  \item{xlab}{Label for the x-axis.}
  \item{ylab}{Label for the y-axis.}
  \item{\dots}{Further arguments for the plot method.}
}
\details{
  This functions permits to display the cumulative hazards estimates in
  multistate models in an original manner. A drawing of the multistate
  model is provided along with the display of the estimated cumulative
  hazards that are put along the arrows that describes the possible
  transitions.

  The multistate model is displayed using the \pkg{Rgraphviz}
  package. By default, the \code{Ragraph} object needed for the display
  is computed internally, letting few options for the user to be
  modified. For a finer control, the user can provide his own
  \code{Ragraph} object using the \code{\link[Rgraphviz]{agopen}}
  function.

  The cumulative hazards plots are positioned by the user using the
  \code{\link[grid]{grid.locator}} function. One has just to click
  where he wants the plot to be. The order of the transitions is
  displayed on the prompt. The \code{width}, \code{height}, \code{angle}
  and \code{just} arguments permit to adjust the way the plots are displayed.
}
\value{
  \code{object} with the invisible flag set
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\note{
  The \pkg{Rgraphviz} package requires the graphviz software to be
  installed.

  \pkg{Rgraphviz} binaries for windows are not available on CRAN, but
  are on bioconductor.
}
\seealso{\code{\link{mvna}}}
\examples{
data(sir.cont)

sir.cont <- sir.cont[order(sir.cont$id, sir.cont$time), ]
for (i in 2:nrow(sir.cont)) {
  if (sir.cont$id[i]==sir.cont$id[i-1]) {
    if (sir.cont$time[i]==sir.cont$time[i-1]) {
      sir.cont$time[i-1] <- sir.cont$time[i-1] - 0.5
    }
  }
}

### Computation of the transition probabilities
# Possible transitions
tra <- matrix(ncol=3,nrow=3,FALSE)
tra[1, 2:3] <- TRUE
tra[2, c(1, 3)] <- TRUE

# mvna
temp <- mvna(sir.cont, c("0", "1", "2"), tra, "cens")

\dontrun{
msmplot(temp, width = c(0.25, 0.25, 0.2, 0.25),
        height = c(0.2, 0.2, 0.18, 0.2), lcex=0.8)
}
}
\keyword{hplot}
\keyword{survival}
