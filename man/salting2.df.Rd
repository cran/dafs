\name{salting2.df}
\alias{salting2.df}
\title{Salting out effects in forensic blood alcohol determination}
\description{Blood alcohol measurements determined by headspace gas
chromatography have been challenged on the grounds that the
presence of the preservative sodium fluoride in blood samples
artificially increases headspace alcohol concentrations due to a
salting out effect. Blood samples containing varying amounts of
ethanol and sodium fluoride (NaF) were tested using semi-automated
headspace gas chromatography with n-propyl alcohol as the
internal standard to assess the validity of this challenge. Miller et al found,
in fact, that under these test conditions the measured alcohol
levels are systematically depressed as the amount of sodium
fluoride in the blood sample increases.

Blood was drawn from each of four subjects at
two time points, first near the time of estimated peak blood
alcohol concentration and then approximately 1.5 hours later.
Samples were initially analyzed with NaF
at manufacturer's levels (ca. 10 mg/mL).
}
\usage{
data(salting2.df)
}
\format{
  A data frame containing four variables
  \tabular{rlll}{
  [,1] \tab subject \tab numeric factor \tab subject identifier 1-4 \cr
  [,2] \tab time \tab numeric factor \tab time sample taken 0 or 1.5h \cr
  [,3] \tab NaF \tab numeric factor \tab the level of sodium fluoride
  added in mg/mL \cr
  [,4] \tab EtOH \tab numeric \tab alcohol concentration in g/100mL \cr
  }
}
\details{
  Note that the blocking and treatment factors in this data frame are
  numeric. Therefore, to use them as such will require the use of \code{factor}
  or \code{ordered}.
}
\references{
B.A. Miller, S.M. Day, T.E. Vasquez, F.M. Evans, Absence of salting out
effects in forensic blood alcohol determination at various
concentrations of sodium fluoride using semi-automated headspace gas
chromatography, Science & Justice, Volume 44, Issue 2, April 2004, Pages
73-76.
}
\author{B. A. Miller et al.}
\keyword{datasets}
