\name{odfList}
\alias{odfList}
\title{Write XML for one layer lists}
\description{
Creates ODF markup for enumerated or bulleted list one layer deep
}
\usage{
odfList(data, styles = NULL)
}
\arguments{
  \item{data}{a vector}
  \item{styles}{a list of style types and their names. Only style types
  of \code{bulletList} or \code{paraText} are required.}
}

\value{
a string
}
\author{Max Kuhn}

\examples{
data(defaultStyles)
odfList(levels(iris$Species), styles = lapply(defaultStyles, function(x) x$name))
}
\keyword{utilities}
