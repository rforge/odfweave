"odfTable.data.frame" <-
function(
   x, 
   colnames    = NULL,  # allow alt col names (what about dimnames[[1]]?)
   useRowNames = TRUE,  # discard or not
   digits      = max(3, getOption("digits") - 3), 
   name = paste("Table", floor(runif(1) * 1000), sep = ""),
   styleNames = NULL,
   ...)
{
   colTypes <- unlist(lapply(x, odfDataType))
   xChar <- as.matrix(format(x, digits = digits, ...))
   if(useRowNames)
   {
      xChar <- cbind(rownames(x), xChar)
      if(!is.null(dimnames(xChar)[[2]])) dimnames(xChar)[[2]] <- c("", dimnames(x)[[2]])
      colTypes <- c("string", colTypes)
   }
   
   if(!is.null(colnames) && length(colnames) != dim(xChar)[2]) 
      stop("wrong length of column names")
   if(!is.null(colnames)) dimnames(xChar)[[2]] <- colnames

   tbleText <- odfTableGen(xChar, colTypes, header = dimnames(xChar)[[2]], tableName = name, styles = styleNames)
   tbleText
}

