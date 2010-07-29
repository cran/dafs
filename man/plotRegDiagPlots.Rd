\name{plotRegDiagPlots}
\alias{plotRegDiagPlots}
\title{Plot some standard regression diagnostic plots}
\description{Produces 3 plots in a 2 x 2 array. The plots are a plot of
  residuals versus predicted values (pred-res plot), a histogram of the
  residuals with a normal distribution superimposed, and a normal
  QQ-plot of the residuals.
}
\usage{
plotRegDiagPlots(fit)
}
\arguments{

  \item{fit}{A \code{lm} object}
}
\seealso{
  plot.lm
}
\examples{
x = runif(100)
y = 3*x+2+dnorm(100)
fit = lm(y~x)
plotRegDiagPlots(fit)
}
\author{J Curran}
\keyword{Book}
