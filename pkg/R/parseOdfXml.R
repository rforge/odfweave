"parseOdfXml" <-
function(x, control)
{
   leadWhite <- function(x) sub ("^[ \t]*", "", sub ("[ \t]*$", "", x)) 
   rCmd <- function(x)
   {
      tmpString <- stripXmlTag(x)
      tmp <- odfTranslate(tmpString)            
      tmp
   }
   
   SexpLines <- grep("\\Sexpr", x, extend = FALSE)
   x[SexpLines] <- odfTranslate(x[SexpLines])

   # search thru for lines with << and >>=
   hasStartTag <- grep("&gt;&gt;=", x, extend = FALSE)
   hasEndTag <- grep("&lt;&lt;", x, extend = FALSE)
   
   startLines <- hasEndTag[hasEndTag %in% hasStartTag]
   hasPlots <- isPlotChunk(x[startLines])
   
   for(i in seq(along = startLines))
   {
      if(i == 1) tmpData <- x[1:startLines[1] - 1]
      tmpVec <- vector(mode = "character", length = length(x) - startLines[i])
      count <- 0
      # from there, search for a line with only a @
      for(j in startLines[i]:length(x))
      {
         count <- count + 1
         rCode <- rCmd(x[j])
         tmpVec[count] <- rCode
         if(rCode == "@")
         {
            tmpVec <- tmpVec[1:count]
            if(hasPlots[i])
            {
               plotName <- paste("rPlot", floor(runif(1) * 10000), ".", control$plotType, sep = "")
               # insert device code around all code in the chunk
               tmpVec <- c(
                  tmpVec[1],
                  figGen(
                  	type = control$plotType,
                  	device = control$plotDevice,
                  	plotName = paste("./Pictures/", plotName, sep = "")), 
                  tmpVec[c(-1, -length(tmpVec))],
                  "dev.off()",
                  tmpVec[length(tmpVec)])
               # now we need to add the xml to display the plot
               tmpVec <- c(
                  tmpVec,
                  odfInsertPlot(plotName, control$figWidth, control$figHeight))
            }
            # append R chunk to existing xml
            tmpData <- c(tmpData, tmpVec)
            # if there is more xml, add it
            if(i < length(startLines))
            {
               tmpData <- c(tmpData, x[(startLines[i] + count):(startLines[i + 1] - 1)])
            } else tmpData <- c(tmpData, x[(startLines[i] + count):length(x)])
            break
         }
      }
   }
   tmpData
}

