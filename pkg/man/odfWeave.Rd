\name{odfWeave}
\alias{odfWeave}
\title{Sweave-like processing of OpenDocument format files}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
odfWeave(file, workDir = tempdir(), verbose = TRUE, style = NULL, zipCmd = c("zip -r $$file$$ . -i *", "unzip -o $$file$$"))
}
\arguments{
  \item{file}{the \code{odt} file created using OpenOffice V2.0 or above.}
  \item{workDir}{a path to a directory where the source file will be unpacked and processed}
  \item{verbose}{a logical: should details be printed?}
  \item{style}{a list of style declarations. See \code{\link{defaultStyles}} for an example.}
  \item{zipCmd}{a string for the zipping/unzipping the \code{odt} file. The token \code{$$file$$} will be gsubbed with the file name.}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
a \code{odt} file with the R output
}
\author{Max Kuhn}

\seealso{\code{\link[utils]{Sweave}}}
\examples{
# put an example form the example directory
}
\keyword{utilities}
