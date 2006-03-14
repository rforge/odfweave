\name{odfTable.character}
\alias{odfTable.character}
\title{Creating a OpenDocument format table from a character vector}
\description{
Creating a OpenDocument format table from a character vector
}
\usage{
odfTable.character(x, horizontal = length(x) > 5, colnames = names(x), name = paste("Table", floor(runif(1) * 1000), sep = ""), styleNames = NULL, ...)
}
\arguments{
  \item{x}{a character vector}
  \item{horizontal}{a logical: should the vector be shown as a 1xn table or nx1?}
  \item{colnames}{a vector of column names that can be used.}
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
cat(odfTable(letters[1:5]))
}
\keyword{utilities}

