\name{vhcdt.df}
\alias{vhcdt.df}
\title{Vitreous Humour Carbohydrate Deficient Transferrin}
\description{
This data has the carbohydrate-deficient transferrin concentration in vitreous humour (VH-CDT) in 21 alcoholics and 7 non-alcoholics.
}
\usage{
data(vhcdt.df)
}
\format{
The data consist of a data frame with 28 observations on 7 variables.
\tabular{rlll}{
  age alc vhcdt1 vhcdt2 vhtf td th
[,1] \tab age \tab integer \tab age in years \cr
[,2] \tab alc \tab factor \tab levels (Y, N) \cr
[,3] \tab vhcdt1 \tab double \tab vitreous humour carbohydrate deficient
transferrin concentration (micrograms per liter) - first assay run \cr
[,4] \tab vhcdt2 \tab double \tab vitreous humour carbohydrate deficient
transferrin concentration (micrograms per liter) second assay run\cr
[,5] \tab vhtf \tab double \tab vitreous humour transferrin (micrograms
per liter)\cr
[,6] \tab td \tab integer \tab time interval to autopsy since found dead
\cr
[,7] \tab td \tab integer \tab time interval to autopsy since found alive \cr
}
vhcdt1 had a detection limit of 5 micrograms per liter. Observations
below this level were coded as 2.5. Similarly in the second assay a
detection limit of 2.5 micrograms per liter was used. Observations below
this threshold were coded as 1.125.
}
\references{
Berkowicz, A., Wallerstedt, S., Wall, K. and Denison, H., Analysis of
carbohydrate-deficient transferrin (CDT) in vitreous humour as a
forensic tool for detection of alcohol misuse, Forensic Science
International 137:(2003) 119-124.
}
\author{Berkowicz, A. et al.}
\keyword{datasets}
