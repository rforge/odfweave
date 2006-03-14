\name{odfTableGen}
\alias{odfTableGen}
\title{Nuts and bolts of table generation}
\description{
Writes the line-by-line ODF table code
}
\usage{
odfTableGen(x, dataType, header = NULL, tableName, styles = NULL)
}
\arguments{
  \item{x}{a character matrix}
  \item{dataType}{a character string of values that specify the kind of data contained in the columns consistent with \code{office:value-type} in the ODF specification. The function \code{\link{odfDataType}} can be used to generate these values.}
  \item{header}{a character string of column headers}
  \item{tableName}{a character string for the table}
  \item{styles}{a character string of style names used}
}

\value{
a character string
}
\author{Max Kuhn}
\examples{
 cat(
    odfTableGen(
       matrix(letters[1:9], 3), 
       dataType = rep("string", 3), 
       tableName = "Test"))
}
\keyword{utilities}

