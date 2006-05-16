\name{odfWeave}
\alias{odfWeave}
\title{Sweave-like processing of OpenDocument format files}
\description{
  Sweave-like processing of OpenDocument format files
}
\usage{
odfWeave(file, dest, workDir = tempdir(), cleanup = FALSE, verbose = TRUE, style = NULL, zipCmd = c("zip -r $$file$$ .", "unzip -o $$file$$"))
}
\arguments{
  \item{file}{the \code{odt} file created using OpenOffice V2.0 or above.}
  \item{dest}{path to put the processed file (should include file name and extension)}  
  \item{workDir}{a path to a directory where the source file will be unpacked and processed}
  \item{cleanup}{a logical: remove the working directory?}
  \item{verbose}{a logical: should details be printed?}
  \item{style}{a list of style declarations. See \code{\link{defaultStyles}} for an example.}
  \item{zipCmd}{a string for the zipping/unzipping the \code{odt} file. The token \code{$$file$$} will be gsub'ed with the file name.}
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
demoFile <- system.file("examples", "demo.odt", package = "odfWeave")
outputFile <- gsub("demo", "processed", demoFile)

workingPath <- tempdir()

library(odfWeave)
data(defaultStyles)
odfWeave(demoFile, outputFile, workingPath, style = defaultStyles, verbose = TRUE)

unlink(workingPath, recursive = TRUE)

}
\keyword{utilities}

