"odfInsertPlot" <-
function(file, height, width, 
   units = "in",
   anchor = c("\n<text:p>\n", "\n</text:p>\n"),
   name = paste("graphics", floor(runif(1) * 1000), sep = ""), 
   copy = TRUE, 
   dest = paste(getwd(), "/Pictures", sep = ""),
   verbose = FALSE)
{

   if(!(getExt(file) %in% c("png", "jpeg", "jpg", "gif", "tiff", "bmp")))
      stop("graphics format not supported")

   if(copy)
   {
      newPath <- paste(dest, "/", basename(file), sep = "")
   
      if(!file.exists(dest))
         stop(paste(dest, "does not exist"))   
      if(verbose) cat("  Copying ", file, " to ", dest, "\n"); flush.console()      
      if(!file.copy(file, newPath,  overwrite = TRUE))
         stop("Error copying file")
   }
   
   
   plotString <- paste(
      "    <draw:frame ",
      "\n",
#      "     draw:style-name=\"fr1\"\n",
      "      draw:name=\"", name, "\"\n",
      "      text:anchor-type=\"paragraph\"\n",
      "      svg:width=\"", width, units, "\"\n",
      "      svg:height=\"", height, units, "\"\n",
      "      draw:z-index=\"0\">\n",
      "      <draw:image\n",
      "         xlink:href=\"Pictures/", basename(file), "\"\n",
      "         xlink:type=\"simple\"\n",
      "         xlink:show=\"embed\"\n",
      "         xlink:actuate=\"onLoad\"/>\n",
      "    </draw:frame>",
      sep = "")
      
   out <- paste(anchor[1], plotString, anchor[2])   
   structure(out, class = "odfInsertPlot")     
}

print.odfInsertPlot <- function(x, ...) cat(x)

