\name{odfInsertPlot}
\alias{odfInsertPlot}
\title{Write XML for image inseration}
\description{
Writes ODF markup to allow images in documents. Also can copy
the image file to the Picture directory.
}
\usage{
odfInsertPlot(file, height, width, units = "in", 
anchor = c("\n<text:p>\n", "\n</text:p>\n"), 
name = paste("graphics", floor(runif(1) * 1000), sep = ""), 
copy = TRUE, dest = paste(getwd(), "/Pictures", sep = ""), verbose = FALSE)
}
\arguments{
  \item{file}{a string for the image file location}
  \item{height}{the dispay height of the image}
  \item{width}{the dispay width of the image}
  \item{units}{the units for the display dimensions}
  \item{anchor}{a character vector that the image markup will be 
  sandwiched between}
  \item{name}{a name for the figure}
  \item{copy}{a logical: should the function copy the image file to
  the picture directory? The function will overwrite files (if needed).}
  \item{dest}{the locaiton of the pciture directory}
  \item{verbose}{a logical: should details be written out?}
}
\value{
a character string
}
\author{Max Kuhn}

\keyword{utilities}
