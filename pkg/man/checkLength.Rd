\name{checkLength}
\alias{checkLength}
\title{Shorten character vector elements}
\description{
This function will look for elements larger than \code{cutoff}. These lines will be broken on blank spaces and re-inserted into the vector at their original location (so the total length of \code{x} may be different after execution).
}
\usage{
checkLength(x, cutoff = 999)
}
\arguments{
  \item{x}{a character vector}
  \item{cutoff}{threshold on number of characters for splitting}
}
\details{
  This function is needed because R can have trouble writing text lines that are longer than 999 characters.
}
\value{
a character vector
}
\author{Max Kuhn}
\examples{
testing <- letters[1:10]
testing[3] <- paste(1:10, collapse = " ")
testing[7] <- paste(1:5, collapse = " ")
checkLength(testing, 4)
}
\keyword{utilities}
