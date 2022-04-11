\name{shadeDens}
\alias{shadeDens}
\title{Add a shaded region to a pdf plot}
\description{
  Useful for shading regions of interest (critical regions perhaps) on a
  theoretical pdf to illustrate concepts such as P-values.
}
\usage{
shadeDens(x0, x1, dens, col = "lightgrey", n.points  = 200,
         lty = 1,...)
}
\arguments{
\item{x0}{A starting x-value for the region to be shaded}
\item{x1}{An ending x-value for the region to be shaded}
\item{dens}{A function that calculates the pdf}
\item{col}{A color for the shaded region}
\item{n.points}{The number of points to calculate the pdf at over the
interval [x0,x1]}
\item{lty}{Line type}
\item{\dots}{Additional arguments to be fed to \code{dens} such as
degrees of freedom, mean or standard deviation}
}
\details{
Adds a filled polygon to an existing pdf plot.
}
\examples{
x = seq(-4.5,4.5,by = 0.01)
plot(x, dnorm(x), type = 'l')
x0 = qnorm(0.975)
x1 = 4.5
shadeDens(x0, x1, dnorm)
}
\author{J Curran}
\keyword{hplot}
