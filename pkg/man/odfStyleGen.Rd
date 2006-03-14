\name{odfStyleGen}
\alias{odfStyleGen}
\title{Generate style ODF XML}
\description{
This takes a list of style declarations and converts it to ODF XML lines that can be wedged between \code{<office:automatic-styles>} and \code{</office:automatic-styles>}.
}
\usage{
odfStyleGen(x)
}
\arguments{
  \item{x}{a list of style declarations for various components (lists, paragraphs etc.). For example, see \code{\link{defaultStyles}}. }
}

\value{
a character vector where each element is a line of XML to be cat'ed into the document.
}

\author{Max Kuhn}

\seealso{\code{\link{defaultStyles}}}
\examples{
data(defaultStyles)
cat(odfStyleGen(defaultStyles))
}
\keyword{utilities}

