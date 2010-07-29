\name{printCI}
\alias{printCI}
\title{Produce a formatted confidence interval}
\description{
  Takes a vector of length two and prints out a confidence interval in a
  user specified format.
}
\usage{
printCI(x, fmt)
}
\arguments{
  \item{x}{A vector of length two containing the lower and upper bounds of a confidence interval}
  \item{fmt}{A format string to be used by \code{sprintf}. Note that
    this format string is repeated twice in the code for each bound of
    the confidence interval.}
}
\value{
  A string containing the formatted CI
}
\seealso{
  sprintf
}
\examples{
n = 100
x = rnorm(n)
mx = mean(x)
se = sd(x)/sqrt(n)
ci = mx + qnorm(c(0.025,0.975))*se
printCI(ci, '\%5.2f')
}
\author{James Curran}
\keyword{Book}
