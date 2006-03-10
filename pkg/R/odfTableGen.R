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
      headLine01 <- paste(
         "\n      <text:p ", 
         textHeaderStyle,
         "> ",  
         header, 
         " </text:p>",
         sep = "")

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
   
   textMat <- apply(x, 2, function(data, stl) paste(
      "\n      <text:p ",
      stl,
      "> ", 
      data, 
      " </text:p>",
      sep = ""),
      stl = textStyle)
   # in case x only had one row, apply will make it a vector
   if(!is.matrix(textMat)) textMat <- as.matrix(t(textMat))
   cellMat <- t(apply(textMat, 1, 
      function(data, dType) paste(
         "\n    <table:table-cell ", 
         tableCellStyle, 
         "office:value-type=\"", 
         dType, 
         "\">", 
         data, 
         "\n    </table:table-cell>", 
         sep = ""), 
      dType = dataType))
   flatCellMat <- apply(cellMat, 1, function(data) paste(data, collapse = "\n"))
   rowVec <- paste("\n  <table:table-row>", flatCellMat, "\n  </table:table-row>")
  
   startText <- paste(
      "\n<table:table table:name=\"",
      tableName,
      "\" ",
      tableStyle,
      ">", 
      "\n  <table:table-column ",
      tableColumnStyle,
      "table:number-columns-repeated=\"",
      length(dataType),
      "\"/>",
      sep = "")
   c(startText, headLine03, rowVec, "\n</table:table>\n\n")
}

