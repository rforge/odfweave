"odfTableGen" <-
function(x, dataType, header = NULL, tableName, styles = NULL)
{
   tableDim <- dim(x)
   
   if(!is.null(styles))
   {
      if(!is.null(styles$table)) tableStyle <- paste(" table:style-name=\"", styles$table, "\" ", sep = "")
         else tableStyle <- ""
      if(!is.null(styles$tableColumn)) tableColumnStyle <- paste(" table:style-name=\"", styles$tableColumn, "\" ", sep = "")
         else tableColumnStyle <- ""
      if(!is.null(styles$tableRow)) tableRowStyle <- paste(" table:style-name=\"", styles$tableRow, "\" ", sep = "")
         else tableRowStyle <- ""
      if(!is.null(styles$tableCell)) tableCellStyle <- paste(" table:style-name=\"", styles$tableCell, "\" ", sep = "")
         else tableCellStyle <- ""
      if(!is.null(styles$cellText)) textStyle <- paste(" text:style-name=\"", styles$cellText, "\" ", sep = "")
         else textStyle <- ""
      if(!is.null(styles$cellHeaderText)) textHeaderStyle <- paste(" text:style-name=\"", styles$cellHeaderText, "\" ", sep = "")
         else textHeaderStyle <- ""      
   } else tableStyle <- tableColumnStyle <- tableRowStyle <- tableCellStyle <- textStyle <- textHeaderStyle <- ""
   
   
   if(!is.null(header))
   {
      headLine01 <- paste("\n      <text:p ", textHeaderStyle, "> ", header, " </text:p>", sep = "")

      headLine02 <- paste(
         "\n    <table:table-cell ", 
         tableCellStyle, 
         "office:value-type=\"string\">", 
         headLine01, 
         "\n    </table:table-cell>", 
         sep = "")

      headLine03 <- paste(
         "\n  <table:table-header-rows>\n    <table:table-row>\n",
         paste(headLine02, collapse = ""),
         "\n    </table:table-row>\n   </table:table-header-rows>\n",
         sep = "")

   } else headLine03 <-  NULL   
   
   
   leftValueTags <- matrix(
      rep(
         paste("      <text:p ",  textStyle, "> ", sep = ""),
         prod(tableDim)),
      nrow = tableDim[1])
   rightValueTags <- matrix(
      rep(
         paste(" </text:p>", sep = ""),
         prod(tableDim)),
      nrow = tableDim[1])      
   valueMarkup <- matrixPaste(leftValueTags, x, rightValueTags,  sep = rep("", 3))
   columnMarkup <- rep(
         paste("    <table:table-cell ", tableCellStyle, "office:value-type=\"", dataType, "\">", sep = ""),
         tableDim[1])
   leftCellTags <- matrix(columnMarkup, nrow = tableDim[1], byrow = TRUE)      
   rightCellTags <- matrix(
      rep(
         paste(" </table:table-cell>", sep = ""),
         prod(tableDim)),
      nrow = tableDim[1])    
   cellMarkup <- matrixPaste(leftCellTags, valueMarkup, rightCellTags, sep = rep("\n", 3))
   leftRowTags <- matrix(
      c(
         rep("<table:table-row>", tableDim[1]),
         rep("", (tableDim[2] - 1) * tableDim[1])),
      nrow = tableDim[1])
   rightRowTags <- matrix(
      c(
         rep("", (tableDim[2] - 1) * tableDim[1]),      
         rep("</table:table-row>\n", tableDim[1])),
      nrow = tableDim[1])   
   rowMarkup <- matrixPaste(leftRowTags, cellMarkup, rightRowTags, sep = rep("\n", 3))
      
   startText <- paste(
      "\n<table:table table:name=\"",  tableName, "\" ", tableStyle, ">", 
      "\n  <table:table-column ",  tableColumnStyle, 
      "table:number-columns-repeated=\"", length(dataType), "\"/>",
      sep = "")

   list(
      start = startText,
      header = headLine03,
      cells = rowMarkup,
      end = "\n</table:table>\n")
}
