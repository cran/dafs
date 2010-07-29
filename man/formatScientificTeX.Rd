\name{formatScientificTeX}
\alias{formatScientificTeX}
\alias{fmtST}
\title{
Format a number in scientific notation into LaTeX.
}
\description{
Format a number in standard scientific format XXX.XXEXXX into a string
that can be typeset by LaTeX
}
\usage{
formatScientificTeX(x, width, digits)
}
\arguments{
  \item{x}{
    The number to be formatted. This number does not need to be in
    scientific format.
}
  \item{width}{
    \code{width} and \code{digits} are interpreted in the same way
    that they are for
    the C function sprintf. The number will be formatted so that it
    takes x times 10 to the power of y, where x is formatted so that the
    number has \code{width} characters in total (including a decimal point) and
    \code{digits} decimal places. 
}
  \item{digits}{
    See \code{width}.
}
}
\details{
If \code{x = 300.123}, \code{width = 4} and \code{digits = 1}, then this
function will return  " 3.0\\\\times 10^{2}"
}
\value{
A string that will format in a LaTeX inline math environment.
}
\author{
James Curran
}
\seealso{
\code{\link{sprintf}}
}
\examples{
fmtST(300.123, 1, 4)
}
