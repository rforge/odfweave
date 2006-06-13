\name{odfTable.numeric}
\alias{odfTable.numeric}
\title{Creating a OpenDocument format table from a numeric vector}
\description{
Creating a OpenDocument format table from a numeric vector
}
\usage{
odfTable.numeric(x, horizontal = length(x) > 5, colnames = names(x), digits = max(3, getOption("digits") - 3), name = paste("Table", floor(runif(1) * 1000), sep = ""), styleNames = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector}
  \item{horizontal}{a logical: should the vector be shown as a 1xn table or nx1?}
  \item{colnames}{a vector of column names that can be used.}
  \item{digits}{number of significant digits passed to format}
  \item{name}{A name for the table. ODF requires a name for each object, so we will make up a random name if left unspecified.}
  \item{styleNames}{A character vector with the names for any styles specified by the user.}
  \item{\dots}{optional arguments that can be passed to \code{format}}
}
\details{
The vector is converted to a matrix and passed to odfTableGen.
}
\value{
a character string of XML
}
\author{Max Kuhn}

\seealso{\code{\link{odfTableGen}}}
\examples{
odfTable(rnorm(4))
}
\keyword{utilities}

