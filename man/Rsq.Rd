\name{Rsq}
\alias{Rsq}
\title{Formats an R-squared figure for LaTeX}
\description{
  Produces a math-mode formatted string for R-squared.
}
\usage{
Rsq(R, fmt = "$R^2 = \%6.4f$", adj = FALSE)
}
\arguments{
  \item{R}{The R-squared value}
  \item{fmt}{A formatting string for LaTeX}
  \item{adj}{If TRUE appends 'adjusted '  to the format string}
}
\value{
  A string that will be correctly formatted by LaTeX
}
\seealso{
  Sweave
}
\examples{
r = 0.99
Rsq(r)
}
\author{J Curran}
\keyword{Book}
