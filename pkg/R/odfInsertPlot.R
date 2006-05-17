"odfInsertPlot" <-
function(file, height, width, 
   units = "in",
   anchor = c("<text:p>", "</text:p>"),
   name = paste("graphics", floor(runif(1) * 1000), sep = ""), 
   verbose = FALSE)
{

   if(!(getExt(file) %in% c("png", "jpeg", "jpg", "gif", "tiff", "bmp")))
      stop("graphics format not supported")
 
   
   plotString <- c(
      "    <draw:frame ",
      paste("      draw:name=\"", name, "\"", sep = ""),
      "      text:anchor-type=\"paragraph\"",
      paste("      svg:width=\"", width, units, "\"", sep = ""),
      paste("      svg:height=\"", height, units, "\"", sep = ""),
      "      draw:z-index=\"0\">",
      "      <draw:image",
      paste("         xlink:href=\"Pictures/", basename(file), "\"", sep = ""),
      "         xlink:type=\"simple\"",
      "         xlink:show=\"embed\"",
      "         xlink:actuate=\"onLoad\"/>",
      "    </draw:frame>")
      
   out <- c(anchor[1], plotString, anchor[2])   
   out   
}


