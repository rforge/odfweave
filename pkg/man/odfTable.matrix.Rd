\name{odfTable.matrix}
\alias{odfTable.matrix}
\title{Creating a OpenDocument format table from a matrix}
\description{
reating a OpenDocument format table from a matrix
}
\usage{
odfTable.matrix(x, colnames = NULL, useRowNames = TRUE, digits = max(3, getOption("digits") - 3), name = paste("Table", floor(runif(1) * 1000), sep = ""), styleNames = NULL, ...)
}

\arguments{
  \item{x}{a matrix}
  \item{colnames}{optional column names for the table}
  \item{useRowNames}{a logical: should the row names be printed in the final table}
  \item{digits}{number of significant digits passed to format (numeric matrices only)}
  \item{name}{A name for the table. ODF requires a name for each object, so we will make up a random name if left unspecified.}
  \item{styleNames}{A character vector with the names for any styles specified by the user.}
  \item{\dots}{optional arguments that can be passed to \code{format}}
}

\value{
a character string of XML
}
\author{Max Kuhn}

\seealso{\code{\link{odfTableGen}}}
\examples{
cat(odfTable(matrix(rnorm(12), 3)))
}
\keyword{utilities}

