# Notes:
#
# rgroup and n.rgroup should have the same length.
# That is true for cgroup and n.cgroup, also.
#
# More sanity checking of the arguments is needed
#
# Currently, cgroup/n.cgroup is ignored unless "header" is not NULL.
# That doesn't seem to make sense to me, and can be easily fixed.

"odfTableGen" <-
function(x, dataType, header = NULL, tableName, styles,
         cgroup = NULL, n.cgroup = NULL, rgroup = NULL, n.rgroup = NULL)
{
   # Modify arguments if rgroup is specified
   if(!is.null(rgroup))
   {
      x <- cbind("", x)
      dataType <- c("string", dataType)
      header <- c("", header)
      # names(header)[1] <- "V1"  # XXX why do the names of "header" matter?

      # XXX This is wrong if the user specified a value for "styles".
      # XXX It was done because "styles" is a function of the dimension
      # XXX of "x", but this isn't a good solution
      styles <- tableStyles(x, useRowNames = FALSE, dimnames(x)[[2]])
   }

   # Generate the cell matrix, which contains the "table:table-cell" elements
   cellMatrix <- genCellMatrix(x, dataType, styles)

   # Process the "rgroup" argument if specified
   if(!is.null(rgroup))
   {
      # Compute "firstCol" which is used to modify the first column of the
      # "cells" element of the return value
      # XXX Should the cell and text styles be hardcoded like this?
      firstCol0 <- paste(
         "<table:table-cell",
         " table:style-name=\"noBorder\"",
         " table:number-rows-spanned=\"", n.rgroup, "\"",
         " office:value-type=\"string\">",
         "<text:p text:style-name=\"ArialCenteredBold\">", rgroup, "</text:p>",
         "</table:table-cell>",
         sep = "")
      firstCol <- rep('<table:covered-table-cell/>', nrow(cellMatrix))
      idx <- c(1, 1 + cumsum(n.rgroup)[-length(n.rgroup)])
      firstCol[idx] <- firstCol0

      # Substitute "firstCol" for the fake column of empty strings
      # that we added previously
      cellMatrix <- cbind(firstCol, cellMatrix[,-1])

      # Adjust cgroup/n.cgroup due to the new column if they were specified
      if(!is.null(cgroup)) {
         cgroup <- c("", cgroup)
         n.cgroup <- c(1, n.cgroup)
      }
   }

   # Complete the generation of the "cells" element by wrapping
   # "cellMatrix" in row tags
   leftRowTags <- matrix(
      c(
         rep("<table:table-row>", nrow(cellMatrix)),
         rep("", (ncol(cellMatrix) - 1) * nrow(cellMatrix))),
      nrow = nrow(cellMatrix))
   rightRowTags <- matrix(
      c(
         rep("", (ncol(cellMatrix) - 1) * nrow(cellMatrix)),
         rep("</table:table-row>\n", nrow(cellMatrix))),
      nrow = nrow(cellMatrix))
   rowMarkup <- matrixPaste(leftRowTags, cellMatrix, rightRowTags,
                            sep = rep("\n", 2))

   # Generate the "header" element, which contains a
   # "table:table-header-rows" element with children
   headLine <- genHeadLine(header, styles, cgroup, n.cgroup)

   # Generate the "start" element, which contains the "table:table"
   # start tag and a complete "table:table-column" tag
   has <- function(x) !is.null(x) && x != ""
   tableStyle <- if(has(styles$table))
      paste(" table:style-name=\"", styles$table, "\" ", sep = "")
   else
      ""
   startText <- paste(
      "\n<table:table table:name=\"",  tableName, "\" ", tableStyle, ">",
      "\n  <table:table-column ",
      "table:number-columns-repeated=\"", length(dataType), "\"/>",
      sep = "")

   # Gather together all the pieces to return as a list
   list(
      start = startText,
      header = headLine,
      cells = rowMarkup,
      end = "\n</table:table>\n")
}

# Generate a character matrix of the same dimensions as "x".
# Each element of this matrix is a partial XML document, with
# a "table:table-cell" element at the root.
#
# To test it by itself, you can call it as follows:
#
#   library(odfWeave)
#   x <- matrix(1:12, nrow=3)
#   xChar <- format(x, digits=3)
#   dataType <- rep('double', ncol(x))
#   styles <- odfWeave:::tableStyles(xChar, useRowNames=FALSE, colnames(x))
#   odfWeave:::genCellMatrix(xChar, dataType, styles)
#
genCellMatrix <- function(x, dataType, styles)
{
   # Function to generate a matrix containing a single repeated value
   # with the specified dimensions
   makeMatrix <- function(text, dims) {
      matrix(rep(text, prod(dims)), nrow = dims[1])
   }

   endQuote <- makeMatrix("\" ", dim(styles$cell))

   # Wrap each element of "x" in "text:p" tags with appropriate
   # "text:style-name" attributes
   textNameStart <- makeMatrix(" text:style-name=\"", dim(styles$text))
   textNameStart <- ifelse(styles$text == "", "", textNameStart)
   textNameEnd <- ifelse(styles$text == "", "", endQuote)
   textName <- matrixPaste(textNameStart, styles$text, textNameEnd,
                           sep = c("", ""))
   textStart <- makeMatrix("      <text:p", dim(styles$text))
   textEnd <- makeMatrix(">", dim(styles$text))
   tagEnd <- makeMatrix("</text:p>\n", dim(styles$text))
   textMatrix <- matrixPaste(textStart, textName, textEnd, x, tagEnd,
                             sep = rep("", 5))  # XXX repeat 4 times?

   # Wrap each element of "textMatrix" in "table:table-cell" tags
   # with appropriate "table:style-name" and "office:value-type" attributes
   cellNameStart <- makeMatrix(" table:style-name=\"", dim(styles$cell))
   cellNameStart <- ifelse(styles$cell == "", "", cellNameStart)
   cellNameEnd <- ifelse(styles$cell == "", "", endQuote)
   cellName <- matrixPaste(cellNameStart, styles$cell, cellNameEnd,
                           sep = c("", ""))
   valueType <- matrix(rep(dataType, each = nrow(x)), nrow = nrow(x))
   valueTypeStart <- makeMatrix(" <table:table-cell office:value-type=\"",
                                dim(styles$cell))
   cellStart <- matrixPaste(valueTypeStart, valueType, endQuote,
                            sep = c("", ""))
   cellEnd <- makeMatrix(">\n", dim(styles$cell))
   tagEnd <- makeMatrix(" </table:table-cell>\n", dim(styles$cell))
   matrixPaste(cellStart, cellName, cellEnd, textMatrix, tagEnd)
}

genHeadLine <- function(header, styles, cgroup, n.cgroup)
{
   if(!is.null(header))
   {
      # Compute the style attributes for the "table:table-cell" and
      # "text:p" elements
      cellHeaderStyle <- paste(" table:style-name=\"", styles$headerCell,
                               "\" ", sep = "")
      cellHeaderStyle <- ifelse(styles$headerCell == "", "", cellHeaderStyle)

      textHeaderStyle <- paste(" text:style-name=\"", styles$header, "\" ",
                               sep = "")
      textHeaderStyle <- ifelse(styles$header == "", "", textHeaderStyle)

      # Compute "firstRow" which is used in the construction of the "header"
      # element of the return value
      if(!is.null(cgroup) && length(cgroup) > 0)
      {
         numHeadCol <- length(cgroup)
         idx <- c(1, 1 + cumsum(n.cgroup)[-length(n.cgroup)])

         firstRow0 <- paste(
            "\n      <text:p ",
            textHeaderStyle[idx],   # XXX probably need new style mechanism
            ">",
            cgroup,
            "</text:p>",
            sep = "")

         firstRow01 <- paste(
             "<table:table-cell ",
             cellHeaderStyle[idx],  # XXX probably need new style mechanism
             " table:number-columns-spanned=\"", n.cgroup, "\"",
             " office:value-type=\"string\">",
             firstRow0,
             "</table:table-cell>",
             sep = "")

         firstRow02 <- rep('<table:covered-table-cell/>', length(cellHeaderStyle))
         firstRow02[idx] <- firstRow01

         firstRow <- paste(
            "<table:table-row>",
            paste(firstRow02, collapse = ""),
            "</table:table-row>\n",
            sep = "")

      } else {
         firstRow <- NULL
      }

      headLine01 <- paste("\n      <text:p ", textHeaderStyle, ">",
                          header, "</text:p>", sep = "")
      headLine02 <- paste(
         "\n    <table:table-cell ",
         cellHeaderStyle,
         "office:value-type=\"string\">",
         headLine01,
         "\n    </table:table-cell>",
         sep = "")
      paste(
         "\n  <table:table-header-rows>\n",
         firstRow,
         "\n    <table:table-row>\n",
         paste(headLine02, collapse = ""),
         "\n    </table:table-row>\n   </table:table-header-rows>\n",
         sep = "")
   } else {
      NULL
   }
}
