\name{odfTranslate}
\alias{odfTranslate}
\title{Translation function from XML to R Code}
\description{
Converts some XML modified characters (such as &gt) to R code (>). Also tries to mistake proof the code by anticipating characters that might be in UTF-8 encoding to R compliant characters (e.g. in some fonts, " will become a UTF-8 character, which R will choke on).
}
\usage{
odfTranslate(x)
}
\arguments{
  \item{x}{a character string}
}

\value{
a character string
}
\author{Max Kuhn}

\examples{
cat(odfTranslate(c("&gt;", "&lt;", "&quot;", "&apos;", "&amp;")))
}
\keyword{utilities}
