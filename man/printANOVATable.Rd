\name{printANOVATable}
\alias{printANOVATable}
\title{Print a ANOVA table for a Normal GLM}
\description{
  Formats the column headings an ANOVA table for a normal GLM using the input of \code{xtable}.
}
\usage{
printANOVATable(xtbl,  sanitize.text.function = function(x){x}, ...)
}
\arguments{
  \item{xtbl}{The output of \code{xtable} applied to the \code{summary}
    output of a normal GLM.}
  \item{sanitize.text.function}{Don't change this}
  \item{\dots}{Extra arguments to be fed to \code{print.xtable}}
}
\value{
  A LaTeX formatted ANOVA table
}
\seealso{
  print.xtable
}
\author{J Curran}
\keyword{Book}
