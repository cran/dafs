\name{fitPvalue}
\alias{fitPvalue}

\title{
Return the P-value from an F-test for a linear model
}
\description{
This functions the P-value from the (null) hypothesis that all of the
linear predictors are zero or not-significant.
}
\usage{
fitPvalue(fit)
}
\arguments{
  \item{fit}{
    a \code{lm} object.
}
}
\details{
  \code{summary} returns an invisible vector called fstatistic that
  contains the F-statistic and the degrees of freedom used to test the
  hypothesis that all of the linear predictors are zero or
  not-significant. This function takes those values and returns the
  appropriate upper tail probability from the F-distribution.
}
\value{
  A P-value
}
\author{
James Curran
}
\seealso{
\code{\link{summary.lm}}
}
\examples{
x = runif(100,1,10)
y = 2 + 3*x + rnorm(100)
fit = lm(y~x)
fitPvalue(fit)
}

