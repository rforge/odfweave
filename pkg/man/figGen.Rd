\name{figGen}
\alias{figGen}
\title{Generate Device Code}
\description{
  This function will generate code to open a graphics device using \code{fig=TRUE}
}
\usage{
figGen(type, device, plotName = NULL, ...)
}

\arguments{
  \item{type}{type of image format (e.g. "png"). Note that pdf format is probably not accepted (it is not in OpenOffice).}
  \item{device}{the R function call to produce the plot. It is usually the same as \code{type}}
  \item{plotName}{a name for the image file}
  \item{\dots}{options to pass to the device call}
}
\details{
  The results of this file are directly inserted in to the R code in the chunk.
}
\value{
a character string
}

\author{Max Kuhn}

\seealso{ \code{\link{odfInsertPlot}}}
\examples{
figGen("jpg", "jpeg",pointsize = 14,  bg = "blue")
}
\keyword{utilities}
