\name{StangleBook}
\alias{StangleBook}
\title{
Stangle a set of files
}
\description{
Stangle more than one file at once. This is a helper function to Stangle all the
chapters in Data Analysis for Forensic Scientists (DAFS) at once. I have put it
into the library for completeness and in the hope that someone else
might find it useful.
}
\usage{
StangleBook(idx = 0:6,
           fileList = paste(paste("Chapter", idx, "/", sep = ""),
                            paste("ch", idx, ".rnw", sep = ""),
                            sep = ""))
}
\arguments{
  \item{idx}{
    There seven chapters in Data Analysis for Forensic
    Scientists. Corresponding to each chapter is a folder named ChapterI
    where I is a number from 0 to 6, and an Sweave file named
    chI.rnw. This parameter lets the user select which subset of files
    need to be run through Stangle
}
  \item{fileList}{
    This parameter may be altered to suit the structure of your book. By
    default it is set to create a list of files that have the same name
    and directory structure as DAFS. The function iterates over a set of
    file names specified by this parameter.
}
}
\value{
No values are returned
}
\author{
James Curran
}
\seealso{
Stangle
}
\keyword{helper}

