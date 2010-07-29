\name{halfnorm}
\alias{halfnorm}
\title{Produce a half normal plot}
\description{
Produce a half normal plot for a fitted \code{lm} or \code{glm}
object. This function should work for any class that implements
\code{residuals}.
}
\usage{
halfnorm(fit)
}
\arguments{
  \item{fit}{
    A \code{lm} or \code{glm} object.
  }
}
\details{
  The absolute value of the residuals are plotted against the positive
  quantiles of the normal distribution. The largest 5 percent of the
  empirical quantiles are labelled to help identify potential outliers.
}
\value{
No values are returned.
}
\author{
J.M. Curran
}
\note{
The labelling of the largest 5 percent is utterly arbitrary.
}

