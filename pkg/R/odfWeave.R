"odfWeave" <-
function(
   file, 
   dest,
   workDir = tempdir(), 
   cleanup = FALSE,
   verbose = TRUE, 
   style = NULL,
   zipCmd = c("zip -r $$file$$ .", "unzip -o $$file$$"))
{
   currentEncoding <- getOption("encoding")
   options(encoding = "UTF-8")
   currentLoc <- getwd()
   # create a temp dir (or have dir specified)
   if(!file.exists(workDir)) 
   {
      if(verbose) cat("  Creating ", workDir, "\n"); flush.console()
      if(!dir.create(workDir, showWarnings = TRUE, recursive = FALSE))
         stop("Error creating workig directory")
   }
   
   if(verbose) cat("  Setting wd\n"); flush.console()
   setwd(workDir)
      
   workingCopy <- paste(workDir, "/", basename(file), sep = "")      
      
   # copy file to the tmp dir       
   if(!file.exists(file))
      stop(paste(file, "does not exist"))   
   if(verbose) cat("  Copying ", file, "\n"); flush.console()      
   if(!file.copy(file, workingCopy, overwrite = TRUE))
      stop("Error copying file")

   # unpack the file 
   zipCmd[2] <- gsub(
      "$$file$$", 
      paste(
         ifelse(version$os == 'mingw32', "\"", ""),
         workDir, 
         "/", 
         basename(file),
         ifelse(version$os == 'mingw32', "\"", ""), 
         sep = ""), 
      zipCmd[2], 
      fixed = TRUE)   
      
   if(verbose) cat("  Decompressing file using", zipCmd[2], "\n"); flush.console()  
   if(system(zipCmd[2]) != 0)
      stop("Error unzipping file")
   
   # remove original file
   if(verbose) cat("\n  Removing ", workingCopy, "\n"); flush.console()      
   if(unlink(workingCopy, recursive = TRUE) == 1) 
      stop("Error removing original file")   
   
   # find xml files
   fileList <- list.files(workDir)
   xmlFiles <- fileList[grep(".xml", tolower(fileList), fixed = TRUE)]
   if(length(xmlFiles) == 0) stop("Something is wrong - no xml") 
   # should make not enough look for specific files
   
   # load xml into list
   xmlContents <- vector(mode = "list", length = length(xmlFiles))
   
   #readLines is more natrual, but shows warnings due to no EOF (OO creates XML wo EOF)
#   for(i in seq(along = xmlContents)) xmlContents[[i]] <- readLines(xmlFiles[i])
   for(i in seq(along = xmlContents)) xmlContents[[i]] <- scan(file(xmlFiles[i], encoding = "UTF-8"), what = "raw", sep = "\n", blank.lines.skip = FALSE, quiet = !verbose )   
   hasTags <- unlist(lapply(
      xmlContents, 
      function(x) length(c(grep("Sexpr", x), grep("&gt;&gt;=", x))) > 0))
     
   if(any(hasTags))
   {
      if(verbose) cat("\n  Sweave tags found in: ", xmlFiles[hasTags], "\n"); flush.console()      
      sweaveFiles <- xmlFiles[hasTags]
      sweaveContents <- xmlContents[hasTags]
      
      for(j in seq(along = sweaveFiles))
      {
         if(verbose) cat("  Removing xml around <<>>= for ", sweaveFiles[j], "\n"); flush.console()           
         if(length(grep("&gt;&gt;=", sweaveContents[[j]])) > 0) sweaveContents[[j]] <- parseOdfXml(sweaveContents[[j]])    
         
             
         if(length(grep("content.xml", sweaveFiles[j], fixed = TRUE)) > 0 & !is.null(style))
         {
            if(verbose) cat("  Looking for style information in content.xml\n"); flush.console()           
         
            locateAutoStart <- grep("<office:automatic-styles>", sweaveContents[[j]], fixed = TRUE)
            locateAutoStop <- grep("</office:automatic-styles>", sweaveContents[[j]], fixed = TRUE)
            if(length(locateAutoStart) > 1 & length(locateAutoStop) > 1)
               stop("there are more that one <office:automatic-styles> or </office:automatic-styles> tags")
            if(length(locateAutoStart) == 0 & length(locateAutoStop) == 1)
            {
               part1 <- sweaveContents[[j]][1:(locateAutoStop - 1)]
               part2 <- sweaveContents[[j]][locateAutoStop:length(sweaveContents[[j]])]
               sweaveContents[[j]] <- c(part1, " <office:automatic-styles>", part2)
               locateAutoStart <- locateAutoStop
               locateAutoStop <- locateAutoStop + 1     
            }
            # add style text here 
            if(verbose) cat("  Spliiting file around office:automatic-styles at line", locateAutoStop, "\n"); flush.console()           
            part1 <- sweaveContents[[j]][1:(locateAutoStop -1)]
            part2 <- sweaveContents[[j]][locateAutoStop:length(sweaveContents[[j]])]
            styleVec <- odfStyleGen(style)            
            sweaveContents[[j]] <- c(part1, styleVec, part2)            

            if(verbose) cat("\n")
         }        
         
         # write processed lines to Rnw file
         if(verbose) cat("  Writing ", sweaveFiles[j], " to ", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n"); flush.console()           
         rnwFile <- file(paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""), "w")
         writeLines(
            sweaveContents[[j]],
            rnwFile)
         close(rnwFile)
         
         #nuke the xml file
         if(verbose) cat("\n  Removing ", sweaveFiles[j], "\n"); flush.console()           
         if(unlink(paste(workDir, "/", sweaveFiles[j], sep = ""), recursive = TRUE) == 1) 
            stop("Error removing xml file file") 
         if(verbose) cat("  Sweaving ",gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n\n"); flush.console()            
         
         #Sweave results to new xml file
         Sweave(
            file =   paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""),
            output = paste(workDir, "/", sweaveFiles[j], sep = ""),
            quiet = !verbose,
            stylepath = FALSE)
         
         # remove sweave file
         if(verbose) cat("  Removing ", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n"); flush.console()           
#         if(unlink(paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""), recursive = TRUE) == 1) 
#            stop("Error removing xml file file") 
         }
   }    
   
   zipCmd[1] <- gsub(
      "$$file$$", 
      paste(
         ifelse(version$os == 'mingw32', "\"", ""),      
         workDir, 
         "/", 
         basename(file), 
         ifelse(version$os == 'mingw32', "\"", ""),         
         sep = ""), 
      zipCmd[1], 
      fixed = TRUE)

   if(verbose) cat("\n\  Packaging file using", zipCmd[1], "\n"); flush.console()      
   if(system(zipCmd[1]) != 0)
      stop("Error zipping file")


   # copy final file to destination      
   if(!file.exists(workingCopy))
      stop(paste(workingCopy, "does not exist"))   
   if(verbose) cat("  Copying ", workingCopy, "\n"); flush.console()      
   if(!file.copy(workingCopy, dest, overwrite = TRUE))
      stop("Error copying file")

   if(verbose) cat("  Resetting wd\n"); flush.console()
   setwd(currentLoc)

   # delete working dir
   if(cleanup)
   {
      if(verbose) cat("  Removing ", workDir, "\n"); flush.console()
      if(unlink(workDir, recursive = TRUE) == 1) stop("Error removing work dir")
   }
   options(encoding = currentEncoding)
   invisible(NULL)
}

