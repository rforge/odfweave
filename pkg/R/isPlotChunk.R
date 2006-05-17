isPlotChunk <- function(x)
{
   if(length(x) > 0)
   {
      chunkHeaders <- strsplit(tolower(gsub("[ \t]", "", stripXmlTag(x))), ",")
      out <- unlist(lapply(chunkHeaders, function(x) length(grep("fig=true", x)) > 0))
   } else out <- NULL
   out
}

#test <- c( 
#   "&lt;&lt;loadLibs, echo   =\tFALSE,results=hide&gt;&gt;=",
#   "&lt;&lt;loadLibs,echo=FALSE,Fig = TRUE,&gt;&gt;=",
#   "&lt;&lt;loadLibs,echo=FALSE,fig=false,&gt;&gt;=")
#   
#isPlotChunk(test)
#  
    
