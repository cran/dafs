\name{shotgun.df}
\alias{shotgun.df}
\title{Shotgun data}
\description{
  In order to test the validity of range-of-fire estimates obtained by
  the application of regression analysis to shotgun pellet patterns, a
  blind study was conducted in which questioned pellet patterns were
  fired at randomly selected ranges between 3.0 and 15.2 m (10 and 50
  ft) with two different 12-gauge shotguns. each firing a different type
  of buckshot cartridge. Test firings at known ranges were also
  conducted with the same weapons and ammunition.
}
\usage{
data(shotgun.df)
}
\format{
  A data frame with 3 variables:
  \tabular{rlll}{
  [,1] \tab range \tab numeric \tab the range in feet of the firing \cr
  [,2] \tab sqrt.area \tab numeric \tab the square root of the area of the smallest rectangle that would enclose the pellet pattern \cr
  [,3] \tab model \tab factor \tab the model of shotgun used in the experiment \cr
}
}
\references{
  Rowe, W.F. and Hanson, S.R. (1985) Range-of-fire estimates from regression
  analysis applied to the spreads of shotgun pellet patterns: Results of
  a blind study, Forensic Science International, 28(3-4): 239-250.
}
\author{J.M. Curran}
\keyword{datasets}
