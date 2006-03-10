\name{odfInsertPlot}
\alias{odfInsertPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
odfInsertPlot(file, height, width, units = "in", anchor = c("\n<text:p>\n", "\n</text:p>\n"), name = paste("graphics", floor(runif(1) * 1000), sep = ""), copy = TRUE, dest = paste(getwd(), "/Pictures", sep = ""), verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ ~~Describe \code{file} here~~ }
  \item{height}{ ~~Describe \code{height} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
  \item{units}{ ~~Describe \code{units} here~~ }
  \item{anchor}{ ~~Describe \code{anchor} here~~ }
  \item{name}{ ~~Describe \code{name} here~~ }
  \item{copy}{ ~~Describe \code{copy} here~~ }
  \item{dest}{ ~~Describe \code{dest} here~~ }
  \item{verbose}{ ~~Describe \code{verbose} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
test <- TRUE
}
\keyword{utilities}
