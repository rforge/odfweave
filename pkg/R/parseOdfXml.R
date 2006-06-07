"parseOdfXml" <-
function(x, control)
{
   leadWhite <- function(x) sub ("^[ \t]*", "", sub ("[ \t]*$", "", x)) 
     
   xml2R <- function(x)
   {
      if(length(grep("<text:line-break/>", x)) >= 1) x <- unlist(strsplit(x, "<text:line-break/>"))  
      tmpString <- stripXmlTag(x)
      tmp <- odfTranslate(tmpString)            
      tmp
   }
   
   SexpLines <- grep("\\Sexpr\\{([^\\}]*)\\}", x)
   x[SexpLines] <- odfTranslate(x[SexpLines])

   # search thru for lines with <<>>=
   startLines <- grep("&lt;&lt;(.*)&gt;&gt;=", x)

   hasPlots <- isPlotChunk(x[startLines])
   

   for(i in seq(along = startLines))
   {
      #rnwOut is the test of the file that Sweave will run on
      if(i == 1) rnwOut <- x[1:startLines[1] - 1]
      
      # create a container for the pure R code
      pureR <- vector(mode = "character", length = 0)
      
      # count will contain the number of lines of xml that
      # contain R code
      count <- 0
          
      # scan the xml until the end of the code chunk (a line with only a @)
      for(j in startLines[i]:length(x))
      {
      
         count <- count + 1
         
         # convert the line of xml to pure R and possible
         # expand the number of lines of pure R, i.e, 
         # length(x) may !=  length(xml2R(x[j))         
         rCode <- xml2R(x[j])
         
         # append this/these lines of R to the rest of
         # the code chunk
         pureR <- c(pureR, rCode)
         
         if(length(grep("^@", rCode)) > 0)
         {         
         
            if(hasPlots[i])
            {
               plotName <- paste("rPlot", floor(runif(1) * 10000), ".", control$plotType, sep = "")
               # insert device code around all code in the chunk
               pureR <- c(
                  pureR[1],
                  figGen(
                  	type = control$plotType,
                  	device = control$plotDevice,
                  	plotName = paste("./Pictures/", plotName, sep = "")), 
                  pureR[c(-1, -length(pureR))],
                  "dev.off()",
                  pureR[length(pureR)])
               # now we need to add the xml to display the plot
               pureR <- c(
                  pureR,
                  odfInsertPlot(plotName, control$figWidth, control$figHeight))
            }         
         
            # append R chunk to existing xml
            rnwOut <- c(rnwOut, pureR)
            
            # if there is more xml between teh end of the chunk
            # and the next chunk, add it
            if(i < length(startLines))
            {
               rnwOut <- c(rnwOut, x[(startLines[i] + count):(startLines[i + 1] - 1)])
            } else rnwOut <- c(rnwOut, x[(startLines[i] + count):length(x)])
            break            
         }
      }
   }
         
   rnwOut
}

