\name{plotBoth}
\alias{plotBoth}
\title{Produce postscript and pdf images simulataneously}
\description{Produce simulataneous postscript and pdf images using a
  user defined plot function.
}
\usage{
plotBoth(plotfn, filename, control = plotBoth.control(), ...)
}
\arguments{
  \item{plotfn}{A function containing the plotting commands}
  \item{filename}{The name of the output file - .eps and .pdf will be appended to the postscript and pdf images respectively}
  \item{control}{The results of \code{plotBoth.control} controls font embedding and plotting.}
  \item{\dots}{Any additional arguments that need to be fed to plotfn}
}
\seealso{
  plotBoth.control
}
\examples{
\dontrun{plotBoth(function(){
                    plot(rnorm(100),rnorm(100))}, 'test')}
}
\author{J.M. Curran}
\keyword{Book}
