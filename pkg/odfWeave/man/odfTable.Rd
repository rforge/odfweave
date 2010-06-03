\name{odfTable}
\alias{odfTable}
\alias{odfTable.data.frame}
\alias{odfTable.matrix}
\alias{odfTable.character}
\alias{odfTable.factor}
\alias{odfTable.numeric}

\title{Create an Open Document Format table}
\description{
Create an Open Document Format table from a data frame, matrix or vector
}
\usage{
odfTable(x, ...)

\method{odfTable}{numeric}(x, horizontal = length(x) < 5, colnames = names(x), 
   digits = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
  
\method{odfTable}{character}(x, horizontal = length(x) < 5, colnames = names(x), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, ...)
   
  
\method{odfTable}{data.frame}(x, colnames = NULL, useRowNames = TRUE, 
   digits = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""), 
   styles = NULL, cgroup = NULL, n.cgroup = NULL, rgroup = NULL,
   n.rgroup = NULL, ...)
   
\method{odfTable}{matrix}(x, colnames = NULL, useRowNames = TRUE, 
   digits = max(3, getOption("digits") - 3),
    name = paste("Table", floor(runif(1) * 1000), sep = ""), 
    styles = NULL, cgroup = NULL, n.cgroup = NULL, rgroup = NULL,
    n.rgroup = NULL, ...)
}
\arguments{
  \item{x}{A vector, matrix or data frame.}
  \item{horizontal}{A logical: should the vector be shown as a 1xn
    table or nx1?
    This is ignored for other data structures.}  
  \item{colnames}{A vector of column names that can be used.
    Note that if the row names are used in the table, this should
    contain an extra element for that column.}
  \item{useRowNames}{A logical: should the row names be printed in the
    final table.}
  \item{digits}{Number of significant digits passed to \code{format}.}
  \item{name}{A name for the table. ODF requires a name for each object,
    so a random name will be used if unspecified.}
  \item{styles}{An optional list of style names for each table element
    (cells, headers etc). See \code{\link{tableStyles}}}
  \item{cgroup}{A character vector specifying column group header names.
    The resulting row will come before the header names specified via the
    \code{colnames} argument.
    \code{cgroup} must be used with the \code{n.cgroup} argument, and be
    of the same length.}
  \item{n.cgroup}{A numeric vector specifying the column span for the
    corresponding element of the \code{cgroup} argument.
    The sum of this vector must be equal to the number of columns in \code{x},
    plus one, if \code{useRowNames} is true and \code{x} actually has row names.
    \code{n.cgroup} must be used with the \code{cgroup} argument, and be
    of the same length.}
  \item{rgroup}{A character vector specifying row group header names.
    \code{rgroup} must be used with the \code{n.rgroup} argument, and be
    of the same length.}
  \item{n.rgroup}{A numeric vector specifying the row span for the corresponding
    element of the \code{rgroup} argument.
    The sum of this vector must be equal to the number of rows in \code{x}.
    \code{n.rgroup} must be used with the \code{rgroup} argument, and be
    of the same length.}
  \item{\dots}{Optional arguments that can be passed to \code{format}.}
}

\details{
The data structures are converted to character matrices using \code{format}.
The \code{justify} and \code{trim} arguments to \code{format} are usually
overridden by the table style options, so those arguments are automatically set
to \code{justify = "none"} and \code{trim = TRUE}. However, if values of these
arguments are passed using the three dots, format will use them (but they
probably won't do anything).

When using \code{odfTable} in a code chunk, the chunk's \code{results}
argument should be set to \code{xml}.

The document formatting.odt in the package's examples directory
illustrates the process of changing the appearance of the table.

The \code{cgroup}, \code{n.cgroup}, \code{rgroup} and \code{n.rgroup}
arguments are patterned after Frank Harrell's \code{Hmisc} package.
}
\value{
a list of character string that contain XML markup
}
\author{Max Kuhn, with contributions from Zekai Otles}

\seealso{\code{\link{tableStyles}} and Frank Harrell's
  \code{\link[Hmisc]{latexTabular}} for latex table}
\examples{
odfTable(iris[1:5,])

# Simple use of cgroup/n.cgroup
data(odfWeaveData)
cgroup <- c("row","C")
n.cgroup <- c(1, 3)
odfTable(mdat, cgroup=cgroup, n.cgroup=n.cgroup)

# Use of cgroup/n.cgroup and rgroup/n.rgroup
carCgroup <- c("", "Compact", "Medium", "Full", "SUV")
carNcgroup <- c(1, 2, 2, 2, 2)
carRgroup <- c("Jan.-Feb.", "Mar.-Apr")
carRngroup <- c(5, 5)
odfTable(carRentInfo$tableCells, cgroup=carCgroup, n.cgroup=carNcgroup,
         rgroup=carRgroup, n.rgroup=carRngroup)
}
\keyword{utilities}
