\name{plotBoth.control}
\alias{plotBoth.control}
\title{Control over plotBoth function}
\description{
  Gives user control over font embedding and plot generation.
}
\usage{
plotBoth.control(genPlots = .genPlots, embedF = .embedF, embedFoptions = .embedFoptions)
}
\arguments{
  \item{genPlots}{TRUE or FALSE depending on whether plotBoth should
    produce a plot}
  \item{embedF}{TRUE or FALSE depending on whether font embedding is
    desired}
  \item{embedFoptions}{A string containing all the font embedding
    options to be sent to Ghostscript}
}
\value{
  A list containing the values of the variables genPlots, embedF, and embedFoptions
}
\author{J Curran}
\keyword{Book}
