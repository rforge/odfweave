figGen <- function(type, device, plotName = NULL, ...)
{
   
   # make up a random name with prefix "rPlot"
   if(is.null(plotName)) plotName <- paste("rPlot", floor(runif(1) * 10000), ".", type, sep = "")
   
   # convert any ... args to a text string (if any)
   dotList <- list(...)
   if(length(dotList) > 0)
   {
      dotList <- lapply(dotList, function(x) if(mode(x) == "character") paste("\"", x, "\"", sep = "") else x)
      dotArgs <- paste(names(dotList), "=", dotList, sep = "")
      dotText <- paste(dotArgs, collapse = ", ")
      dotText <- paste(", ", dotText, sep = "")
   } else dotText <- ""

   out <- paste(
      device,
      "(filename = \"", 
      plotName, 
      "\"",
      dotText, 
      ")", 
      sep = "")

   out
}

#cat(figGen(), "\n")
#cat(figGen("png", height= 9, width = "cm"), "\n")
#cat(figGen("pdf", height= 9, version = "1.4"), "\n")

# should we generate the xml (from odfInsertPlot) here to go after the Rchunk?

# alter parseOdfXml to look for fig = TRUE (or whatever) and append these
# commands and "dev.off" into the current xml stack?
