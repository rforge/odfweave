"parseOdfXml" <-
function(x)
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
   
   for(i in seq(along = startLines))
   {
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
            x[startLines[i]: (startLines[i] + count - 1)] <- tmpVec
            break
         }
      }
   }
   x
}

