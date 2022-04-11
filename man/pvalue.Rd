\name{pvalue}
\alias{pvalue}
\title{Formats an P-value figure for LaTeX}
\description{
  Produces a math-mode formatted string for a P-value.
}
\usage{
pvalue(p, fmt = "$P = \%6.4f$")
}
\arguments{
  \item{p}{The P-value}
  \item{fmt}{A formatting string for LaTeX}
}
\value{
  A string that will be correctly formatted by LaTeX
}
\seealso{
  Sweave
}
\examples{
p = 0.04
pvalue(p)
}
\author{J Curran}
\keyword{Book}
