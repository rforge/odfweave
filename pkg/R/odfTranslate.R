"odfTranslate" <-
function(x)
{
   x <- gsub("&gt;", ">", x)
   x <- gsub("&lt;", "<", x)
   x <- gsub("&quot;", "\"", x)
   x <- gsub("&apos;", "\'", x)
   x <- gsub("&amp;", "&", x)  
   x <- gsub("“", "\"", x)  
   x <- gsub("”", "\"", x)        
   x
}

