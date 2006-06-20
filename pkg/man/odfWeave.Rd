\name{odfWeave}
\alias{odfWeave}
\title{Sweave processing of OpenDocument format files}
\description{
  Sweave processing of OpenDocument format files
}
\usage{
odfWeave(file, dest, workDir = tempdir(), control = odfWeaveControl())
}
\arguments{
  \item{file}{the ODF file created using OpenOffice V2.0 or above.}
  \item{dest}{path to put the processed file (should include file name and extension)}  
  \item{workDir}{a path to a directory where the source file will be unpacked and processed}
  \item{control}{a list of control settings.  See \code{\link{odfWeaveControl}} for the names of the settable control values and their effects.}
}
\details{
The \code{odfWeave} package was created so that the functionality of \code{\link[utils]{Sweave}} can used to generate documents that the end--user can easily edit. 

The markup language used is the Open Document Format (ODF), which is an open, non--proprietary format that encompasses text documents, presentations and spreadsheets. Version 1.0 of the specification was finalized in May of 2005. One year later, the format was approved for release as an ISO and IEC International Standard. 

There are several editors/office suites that can produce ODF files. OpenOffice is a free, open source editor that, as of version 2.0, uses ODF as the default format. \code{odfWeave} has been tested with OpenOffice to produce test documents. As of the current version, \code{odfWeave} processing of presentations and spreadsheets should be considered to be experimental (but should be supported in subsequent versions). OpenOffice can be used to export the document to MS Word, rich text format, HTML, plain text or pdf formats.

The functionality of \code{\link[utils]{Sweave}} is mostly preserved in \code{odfWeave}, such as weaving, hooks, figure environments, etc. Some functionality, such as writing output to separate files for each code chunk using the \code{split} argument, doesn't make sense when using ODF. As another example, ODF supports a broad range of image formats, so \code{pdf} or \code{eps} arguments to code chunks are somewhat limiting.

The image format and sizes are specified within the \code{\link{odfWeaveControl}} object. The dimensions of the image file and the dimensions of the rendered image can be set independently. See \code{\link{odfWeaveControl}} for more details.

}
\value{
a \code{odt} file with the R output
}
\author{Max Kuhn}

\seealso{ \code{\link{odfWeaveControl}},  \code{\link{RweaveOdf}}, \code{\link[utils]{Sweave}}}
\examples{
demoFile <- system.file("examples", "basics.odt", package = "odfWeave")
outputFile <- gsub("basics", "output", demoFile)
 
workingPath <- tempdir()

library(odfWeave)
odfWeave(demoFile, outputFile, workingPath)

unlink(workingPath, recursive = TRUE)

}
\keyword{utilities}

