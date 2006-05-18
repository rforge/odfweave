"odfTranslate" <-
function(x)
{
   x <- gsub("&gt;", ">", x)
   x <- gsub("&lt;", "<", x)
   x <- gsub("&quot;", "\"", x)
   x <- gsub("&apos;", "\'", x)
   x <- gsub("&amp;", "&", x)  
   x <- gsub("\342\200\235", "\"", x)  
   x <- gsub("\342\200\234", "\"", x)        
   x
}

