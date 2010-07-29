\name{kent.df}
\alias{kent.df}
\title{Chest deflection tolerance}
\description{In this data set are the results of 93 human cadaver crash
  tests. The tests were used in the development of thoracic injury risk
  functions with consideration of age and restraint condition. The data
  can be used with logistic regression models by recoding the variable
  fracture into <6 and >=6, or with a Poisson/quasi-Poisson/negative
  binomial GLM.}
\usage{
data(kent.df)
}
\value{
  A data frame with ... variables
}
\references{
  Kent, R. and Petrie, J., Chest deflection tolerance to blunt anterior
  loading is sensitive to age but not load distribution , Forensic
  Science International 149(2004):2-3 p.121-128.
}
\examples{
data(kent.df)

##recode the response fracture to minor injury (<6 rib fractures) and
##severe injury (>=6 rib fractures)
##kent.df = within(kent.df, {
##                    injury = factor(ifelse(fractures<6,'minor','severe'),
##                                    levels = c('severe','minor'))})

## fit a binomial GLM

## kent.fit = glm(injury~cmax*
}
\author{Kent. R, and Petrie, J.}
\keyword{datasets}
