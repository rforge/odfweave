stripXmlTag <- function(x, leadWhite = FALSE, endWhite = FALSE)
{
   # annihilate anything between a < and a >
   x <- gsub("<[^>]*>", "", x)
   if(!leadWhite) x <- sub("^[ \t]+", "", x)  
   if(!endWhite) x <- sub("[ \t]+$", "", x)  
   x
}

