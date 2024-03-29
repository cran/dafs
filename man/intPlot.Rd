\name{intPlot}
\alias{intPlot}
\alias{intPlot.default}
\alias{intPlot.formula}
\title{Modified Interactions Plot for Two-way Analysis of Variance}
\usage{
intPlot(y, \dots)
\method{intPlot}{default} (y, 
                           fac1 = NULL, 
                           fac2 = NULL, 
                           xlab = NULL, 
                           xlab2 = NULL, 
                           ylab = NULL, 
                           data.order = TRUE, 
                           exlim = 0.1, 
                           jitter = 0.02, 
                           conf.level = 0.95, 
                           interval.type = "tukey", 
                           pooled = TRUE, 
                           tick.length = 0.1,
                           interval.distance = 0.2,
                           col.width = 2/3, 
                           xlab.distance = 0.1, 
                           xlen = 1.5, 
                           ylen = 1,
                           \dots)
\method{intPlot}{formula} (y, 
                           data,
                           xlab = NULL,
                           xlab2 = NULL,
                           ylab = NULL,
                           data.order = TRUE,
                           exlim=0.1,
                           jitter=0.02,
                           conf.level=0.95, 
                           interval.type = "tukey",
                           pooled = TRUE,
                           tick.length = 0.1,
                           interval.distance = 0.2,
                           col.width = 2/3,
                           xlab.distance = 0.1,
                           xlen=1.5,
                           ylen = 1,
                           \dots)
}
\arguments{
 \item{y}{either a formula of the form: y~fac1+fac2 where y is the
   response and fac1 and fac2 are the two explanatory variables used as
   factors, or a single response vector}
   \item{fac1}{if 'y' is a vector, then fac1 contains the levels of factor
     1 which correspond to the y value}
   \item{fac2}{if 'y' is a vector, then fac1 contains the levels of factor
   2 which correspond to the y value}
	\item{data}{an optional data frame containing the variables in the model.}
	\item{xlab}{an optional label for the x-axis. If not specified the
   name of fac1 will be used.}
 \item{xlab2}{an optional label for the lines. If not specified the
   name of fac2 will be used.}
 \item{ylab}{An optional label for the y-axis. If not specified the
   name of y will be used.}
 \item{data.order}{if TRUE the levels of fac1 and fac2 will be set to
   unique(fac1) and unique(fac2) respectively.}
 \item{exlim}{provide extra limits.}
 \item{jitter}{the amount of horizontal jitter to show in the plot.
    The actual jitter is determined as the function is called,
    and will likely be different each time the function is used.}
 \item{conf.level}{confidence level of the intervals.}
 \item{interval.type}{four options for intervals appearing on plot:
   "tukey", "hsd", "lsd" or "ci".}
 \item{pooled}{two options: pooled or unpooled standard deviation used
   for plotted intervals.}
 \item{tick.length}{size of tick, in inches.}
 \item{interval.distance}{distance, as a fraction of the column width,
    between the points and interval.  This is in addition to the
    extra space allocated for the jitter.}
 \item{col.width}{width of a factor `column', as a fraction of the
    space between the centres of two columns.}
 \item{xlab.distance}{distance of x-axis labels from bottom of
    plot, as a fraction of the overall height of the plot.}
 \item{xlen, ylen}{xxx}
 \item{\dots}{optional arguments.}
}
\description{
  This is a modified version of the function \code{interactionPlots}
  from the s20x library which produces greyscale plots.
  
 Displays data with intervals for each
 combination of the two factors and shows the mean differences between
 levels of the first factor for each level of the second factor. Note
 that there should be more than one observation for each combination of
 factors.}

\examples{
library(s20x)
data(mtcars)
intPlot(wt~vs+gear, mtcars)
}

\keyword{hplot}

