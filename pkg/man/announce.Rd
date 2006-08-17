\name{announce}
\alias{announce}
\title{cat and flush console}
\description{
This function is a wrapper for cat with an immediate flush of the console}
}
\usage{
announce(verbose = TRUE, ...)
}
\arguments{
  \item{verbose}{a logical for printing}
  \item{\dots}{arguments passed to cat}
}

\value{
\code{NULL}, invisibly
}
\author{Nathan Coulter}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
announce()
announce(FALSE, "this will not be printed")
announce(TRUE, "but this will be")

}
\keyword{utilities}
