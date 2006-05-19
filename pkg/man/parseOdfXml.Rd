\name{parseOdfXml}
\alias{parseOdfXml}
\title{Find and process R code in XML files}
\description{
This function scans an XML file and determines if there are \code{<<>>=} chunks or \code{Sexpr} tags. The start and end of \code{<<>>=} chunks are determined and the R code is "repaired" in the case where OpenOffice changed the code.
}
\usage{
parseOdfXml(x, control)
}
\arguments{
  \item{x}{a character vector that contains lines of XML}
  \item{control}{a list of control settings.  See \code{\link{odfWeaveControl}} for the names of the settable control values and their effects.}  
}
\value{
a character vector that contains lines of XML that contain valid R code chunks.
}
\author{Max Kuhn}

\keyword{utilities}

