\name{printRegTable}
\alias{printRegTable}
\title{Print a regression table}
\description{
  Formats the column headings a regression table using the input of \code{xtable}.
}
\usage{
printRegTable(xtbl,  sanitize.text.function = function(x){x},
                 test = 't', ...)
}
\arguments{
  \item{xtbl}{The output of \code{xtable} applied to the \code{summary}
    output of a normal GLM.}
  \item{sanitize.text.function}{Don't change this}
  \item{test}{"t" or "z" depending on the desired test}
  \item{\dots}{Extra arguments to be fed to \code{print.xtable}}
}
\value{
  A LaTeX formatted regression table
}
\seealso{
  print.xtable
}
\author{J Curran}
\keyword{Book}
