\name{pairsDAFS}
\alias{pairsDAFS}
\title{Modified Pairwise Scatter Plots with Histograms and Correlations}
\usage{
pairsDAFS(x, ...)
}

\arguments{
  \item{x}{a data frame.}
  \item{\dots}{optional argumments which are passed to the generic pairs
  function.}
}
\description{This is a modified verison of the \code{pairs20x} function
  from the s20x library which produces greyscale plots.

  Plots pairwise scatter plots with histograms and correlations for the data frame.}

\value{Returns the plots.}


\examples{
##peruvian indians
library(s20x)
data(peru.df)
pairsDAFS(peru.df)
}

\keyword{hplot}

