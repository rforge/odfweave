\name{defaultStyles}
\alias{defaultStyles}
\docType{data}
\title{A list of specifications for an Writer document}
\description{
A list with elements \code{paraText}, \code{cellText}, \code{cellHeaderText}, 
etc to specify how tables, text and other items should be formatted. 
}
\usage{data(defaultStyles)}

\details{
}

\references{
  The Opendocument format specification http://xml.coverpages.org/OpenDocumentV10-Standard.html
}
\examples{
# get the element names to pass to functions:
data(defaultStyles)
lapply(defaultStyles, function(x) x$name)
}
\keyword{datasets}
