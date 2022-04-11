\name{salting1.df}
\alias{salting1.df}
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

Blood was drawn from each of six subjects near the time of estimated peak blood
alcohol concentration. Each subject had three blood samples taken to
which 0, 5, and 10mg/mL of NaF were added. The blood alcohol
concentration for each tube was determined twice for each tube
}
\usage{
data(salting1.df)
}
\format{
  A data frame containing four variables
  \tabular{rlll}{
  [,1] \tab subject \tab numeric factor \tab subject identifier 1-6 \cr
  [,2] \tab tube \tab numeric factor \tab tube 1,2,3 for each subject
  \cr
  [,3] \tab rep \tab numeric factor \tab levels 1,2 indicating replicate
  measurement \cr
  [,4] \tab NaF \tab numeric factor \tab the level of sodium fluoride
  added in mg/mL \cr
  [,5] \tab conc \tab numeric \tab alcohol concentration in g/100mL \cr
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
