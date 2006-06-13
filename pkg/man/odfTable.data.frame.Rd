\name{odfTable.data.frame}
\alias{odfTable.data.frame}
\title{Creating a OpenDocument format table from a data frame}
\description{
Creating a OpenDocument format table from a data frame
}
\usage{
odfTable.data.frame(x, colnames = NULL, useRowNames = TRUE, digits = max(3, getOption("digits") - 3), name = paste("Table", floor(runif(1) * 1000), sep = ""), styleNames = NULL, ...)
}
\arguments{
  \item{x}{a data frame}
  \item{colnames}{a vector of column names that can be used. Note that if the row names are used in teh table, this should contain an extra element for that column.}
  \item{useRowNames}{a logical: should the row names be printed in the final table}
  \item{digits}{number of significant digits passed to format}
  \item{name}{A name for the table. ODF requires a name for each object, so we will make up a random name if left unspecified.}
  \item{styleNames}{A character vector with the names for any styles specified by the user.}
  \item{\dots}{optional arguments that can be passed to \code{format}}
}
\details{
The data frame is converted to a character matrix and passed to odfTableGen.
}
\value{
a character string of XML
}
\author{Max Kuhn}

\seealso{\code{\link{odfTableGen}}}
\examples{
odfTable(iris[1:5,])
}
\keyword{utilities}

