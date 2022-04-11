\name{printANODEVTable}
\alias{printANODEVTable}
\title{Print an analysis of deviance (ANODEV) table for a GLM}
\description{
  Formats the column headings an  table for a GLM using the input of \code{xtable}.
}
\usage{
printANODEVTable(xtbl,  sanitize.text.function = function(x){x},
                 test = NULL, ...)
}
\arguments{
  \item{xtbl}{The output of \code{xtable} applied to the \code{summary}
    output of a normal GLM.}
  \item{sanitize.text.function}{Don't change this}
  \item{test}{"Chisq" or "F" depending on the desired test}
  \item{\dots}{Extra arguments to be fed to \code{print.xtable}}
}
\value{
  A LaTeX formatted ANODEV table
}
\seealso{
  print.xtable
}
\author{J Curran}
\keyword{Book}
