\name{abaz.df}
\alias{abaz.df}
\title{DNA from drinking containers}
\description{The amount of DNA left on different types of drinking containers.}
\usage{
data(abaz.df)
}
\format{
  A data frame with 21 columns.
  \tabular{lrrr}{
    [,1] \tab person \tab factor \tab A label (A..F) for the 6
    experimental subjects \cr
    [,2] \tab sample \tab factor \tab A treatement factor indicating the
    different beverage/container combinations \cr
    [,3] \tab ab.sample \tab factor \tab An abbreviated treatment label
    \cr
    [,4] \tab time \tab factor \tab time when DNA concentration was
    measured. Levels: 24hrs, 48hrs \cr
    [,5] \tab amylase \tab numeric \tab the relative amount of
    alpha-amylase activity \cr
    [,6] \tab quant \tab numeric \tab \cr
    [,7] \tab amp.volume \tab numeric \tab \cr
    [,8] \tab dna.conc \tab numeric \tab \cr
    [,9] \tab gel.profile \tab factor \tab \cr
    [,10] \tab failed.profile \tab factor \tab the failure or success of obtaining a usable DNA profile\cr
    [,11:21] \tab d3..fga \tab numeric \tab Total peak heights at each
    locus \cr
  }
}
\references{
  Abaz, J., Walsh, S.J., Curran, J.M., Moss, D.S., Cullen, J., Bright,
  J.A., Crowe, G.A., Cockerton, S.L. and Power, T.E. 'Comparison of the
  variables affecting the recovery of DNA from common drinking
  containers' Forensic Sci Int. 2002 May 23;126(3):233-40.
}
\author{Abaz et al.}
\keyword{datasets}
