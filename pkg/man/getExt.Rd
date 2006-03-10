\name{getExt}
\alias{getExt}
\title{Gets the extension (if there is one) of a file}
\description{
Parses a file to look for the text after the last period
}
\usage{
getExt(x)
}
\arguments{
  \item{x}{a text string of the file (possibly including the path)}
}
\details{
If there is no text after the period (or there is no period), a 
value of "" is retuned
}
\value{
A character vector of extensions. If no extension exists, an \code{NA}
is returned.
}
\author{Max Kuhn}
\seealso{\code{\link{basename}}}
\examples{
getExt(c("/home/me/foo.Rd", "/home/me/foo.bar.Rd"))
getExt("/home/me/foo.bar.Rd")
getExt("/home/me/")
getExt(c("/home/me/foo.Rd", "/home/me/"))
}
\keyword{utilities}
