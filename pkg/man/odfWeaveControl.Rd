\name{odfWeaveControl}
\alias{odfWeaveControl}

\title{Control odfWeave options}
\description{
Allows the user to specify how odfWeave operates and style information for the document.
}
\usage{
odfWeaveControl(zipCmd = c("zip -r $$file$$ .", "unzip -o $$file$$"), cleanup = FALSE, style = defaultStyles, verbose = TRUE, plotType = "png", plotDevice = ifelse(.Platform$OS.type == "windows", "png", "bitmap"), figHeight = 5, figWidth = 5)
}
\arguments{
  \item{zipCmd}{a string for the zipping/unzipping the \code{odt} file. The token \code{$$file$$} will be gsub'ed with the file name.}
  \item{cleanup}{a logical: remove the working directory?}
  \item{style}{a list of style declarations. See \code{\link{defaultStyles}} for an example.}
  \item{verbose}{a logical: should details be printed?}
  \item{plotType}{the image type passed to \code{\link{figGen}}}
  \item{plotDevice}{the R device passed to \code{\link{figGen}} }
  \item{figHeight}{height of the image as displayed in the document}
  \item{figWidth}{width of the image as displayed in the document}
}

\value{
a list with element for each of the arguments above. 
}
\author{Max Kuhn}

\seealso{\code{\link{odfWeave}}, \code{\link{figGen}}, \code{\link{defaultStyles}}}
\examples{
data(defaultStyles)
odfWeaveControl(cleanup = TRUE)
}
\keyword{utilities}
