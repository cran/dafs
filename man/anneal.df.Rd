\name{anneal.df}
\alias{anneal.df}
\title{Annealing float glass}
\description{
  Rushton was interested in the effect of annealing on the refractive
  index of glass. It is well known that annealing float glass changes
  the refractive index (RI). The change in RI - called delta-RI - can
  tell a forensic scientist something about the glass that they are
  examining.

  In this data set, 3 replicate measurements were made from 150 squares
  of glass from a single pane. Each fragment was measured pre and post
  annealing.
}
\usage{
data(anneal.df)
}
\format{
    A data frame with 900 observations on 3 variables.
\tabular{rlll}{
[,1] \tab ri \tab numeric \tab The fragment's refractive index  \cr
[,2] \tab temp  \tab numeric \tab The fragment's match temperature -
  this will be almost perfectly correlated with ri \cr
[,3] \tab anneal  \tab factor \tab either pre or post for pre or post
  annealing \cr
}
}
\references{
  Rushton, K.P., Analysis of the variation of glass refractive index
  with respect to annealing, (2009) MSc Thesis, Forensic Science,
  University of Auckland.
}
\author{K.P. Rushton}
\keyword{datasets}
