\name{stripXmlTag}
\alias{stripXmlTag}
\title{Strips text within "<" and ">"}
\description{
This function takes a character vector and removes any text between "<" and ">". Optionally, it will also remove leading or trailing whitespace. 
}
\usage{
stripXmlTag(x, leadWhite = FALSE, endWhite = FALSE)
}
\arguments{
  \item{x}{a character vector}
  \item{leadWhite}{a logical for removing leading white space}
  \item{endWhite}{a logical for removing tailing white space}
}

\value{
a character vector
}
\author{Max Kuhn and Nathan Coulter}

\examples{
stripXmlTag("   <text:p text:style-name=\"P3\"><text:s text:c=\"3\"/>lm(y~x)</text:p>")
}
\keyword{utilities}
