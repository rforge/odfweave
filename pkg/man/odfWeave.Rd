\name{odfWeave}
\alias{odfWeave}
\title{Sweave-like processing of OpenDocument format files}
\description{
  Sweave-like processing of OpenDocument format files
}
\usage{
odfWeave(file, dest, workDir = tempdir(), control = odfWeaveControl())
}
\arguments{
  \item{file}{the \code{odt} file created using OpenOffice V2.0 or above.}
  \item{dest}{path to put the processed file (should include file name and extension)}  
  \item{workDir}{a path to a directory where the source file will be unpacked and processed}
  \item{control}{a list of control settings.  See \code{\link{odfWeaveControl}} for the names of the settable control values and their effects.}
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
demoFile <- system.file("examples", "basics.odt", package = "odfWeave")
outputFile <- gsub("basics", "output", demoFile)
 
workingPath <- tempdir()

library(odfWeave)
data(defaultStyles)
odfWeave(demoFile, outputFile, workingPath)

unlink(workingPath, recursive = TRUE)

}
\keyword{utilities}

