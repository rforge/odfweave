\name{odfDataType}
\alias{odfDataType}
\title{Determine object type consistent with ODF specification}
\description{
This function converts R storage mode to types constistent with
\code{office:value-type} in ODF.
}
\usage{
odfDataType(x)
}
\arguments{
  \item{x}{a vector}
}
\details{
Right now, only value types of "string" and "double" are impimented (ODF
has other values, such as float, double, percentage, currency, date, 
time, boolean, string, void). More to follow.
}
\value{
a string.
}
\references{  The Opendocument format specification 
http://xml.coverpages.org/OpenDocumentV10-Standard.html
}
\author{Max Kuhn}
\examples{
lapply(iris, odfDataType)
}
\keyword{utilities}
