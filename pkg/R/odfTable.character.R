"odfTable.character" <-
function(
   x, 
   horizontal = length(x) > 5,
   colnames    = names(x),  # allow alt col names (what about dimnames[[1]]?)
   digits      = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""),
   styleNames = NULL,
   ...)
{
   xMat <- if(horizontal) as.matrix(t(x)) else as.matrix(x)
   colTypes <- apply(xMat, 2, odfDataType)   
   xChar <- format(xMat, digits = digits, ...)
   tbleText <- odfTableGen(xChar, colTypes, header = colnames, tableName = name, styles = styleNames)
   tbleText 
}

