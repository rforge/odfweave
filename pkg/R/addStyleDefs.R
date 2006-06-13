addStyleDefs <- function(x, style, verbose = TRUE)
{
   # we need to add our style info between <office:styles> tags
   # which may not exist
   if(verbose) cat("  Looking for style information in styles.xml\n"); flush.console()           
   locateAutoStart <- grep("<office:styles>", x, fixed = TRUE)
   locateAutoStop <- grep("</office:styles>", x, fixed = TRUE)
   if(length(locateAutoStart) > 1 & length(locateAutoStop) > 1)
      stop("there are more that one <office:styles> or <office:styles> tags")
      
   # for some files, there may only be the closing style tag
   # we need to create the opening tag and note the locations   
   if(length(locateAutoStart) == 0 & length(locateAutoStop) == 1)
   {
      part1 <- x[1:(locateAutoStop - 1)]
      part2 <- x[locateAutoStop:length(x)]
      x <- c(part1, " <office:styles>", part2)
      locateAutoStart <- locateAutoStop
      locateAutoStop <- locateAutoStop + 1     
   } 
   if(verbose) cat("  Splitting file around <office:styles> at line", locateAutoStop, "\n"); flush.console()           
   part1 <- x[1:(locateAutoStop -1)] # xml before <office:styles>
   part2 <- x[locateAutoStop:length(x)] # xml after </office:styles>
   styleVec <- odfStyleGen(style)            
   if(verbose) cat("\n")
  
   c(paste(part1, "\n"), styleVec, paste(part2, "\n"))   

}

