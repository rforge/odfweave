"odfWeave" <- function(file, dest, workDir=tempdir(), control=odfWeaveControl()){ 

   #check for an unzipping utility
   if(all(control$zipCmd == c("zip -r $$file$$ .", "unzip -o $$file$$")))
   {
      errorText <- paste(
         "unzip could not be found.",
         "If installed, check your path.",
         "If not installed, either go to",
         "www.info-zip.org and install or",
         "use another utility (like jar)")
      if(.Platform$OS.type == "windows")
      {
         zipTest <- class(try(system("unzip", intern=TRUE, invisible=TRUE),
            silent=TRUE))
         if(class(zipTest) == "try-error") stop(errorText)
      } else {
         zipTest <- system("unzip", intern = TRUE)[1]
         if(is.na(zipTest) || length(grep("UnZip", zipTest)) == 0)
            stop(errorText)
      }
   }

   currentLoc <- getwd()

   #these aren't used in the current implementation
   #currentLocale <- c(Sys.getlocale("LC_CTYPE"), Sys.getlocale("LC_COLLATE"))
   #Sys.setlocale("LC_CTYPE", "C")
   #Sys.setlocale("LC_COLLATE", "C")

   # create a temp dir (or have dir specified)
   if(!file.exists(workDir))
   {
      announce("  Creating ", workDir, "\n")
      announce(workDir, showWarnings = TRUE, recursive = FALSE)
         stop("Error creating working directory")
   }

   announce("  Setting wd\n")
   setwd(workDir)

   workingCopy <- paste(workDir, "/", basename(file), sep = "")

   # copy file to the tmp dir
   if(!file.exists(file)) stop(paste(file, "does not exist"))
   announce("  Copying ", file, "\n")
   if(!file.copy(file, workingCopy, overwrite = TRUE))
      stop("Error copying odt file")

   # unpack the file
   zipCmd <- control$zipCmd
   zipCmd[2] <- gsub(
      "$$file$$",
      paste(
         ifelse(.Platform$OS.type == "windows", "\"", ""),
         workDir, "/", basename(file),
         ifelse(.Platform$OS.type == "windows", "\"", ""),
         sep = ""),
      zipCmd[2],
      fixed = TRUE)

   announce("  Decompressing ODF file using", zipCmd[2], "\n")
   if(.Platform$OS.type == "windows")
   {
      if(system(zipCmd[2], invisible = TRUE) != 0) stop("Error unzipping file")
   } else {
      if(system(zipCmd[2]) != 0) stop("Error unzipping odt file")
   }

   # remove original file
   announce("\n  Removing ", workingCopy, "\n")
   if(unlink(workingCopy, recursive = TRUE) == 1)  stop("Error removing original file")

   #check for Pictures directory
   if(!file.exists(paste(workDir, "/Pictures", sep = "")))
   {
      announce("  Creating a Pictures directory\n")
      picDir <- dir.create(paste(c(workDir, "/Pictures"), collapse = ""), showWarnings = TRUE, recursive = FALSE)
      if(!picDir)  stop("Error creating Pictures directory")
   }

   # find xml files
   fileList <- list.files(workDir)
   xmlFiles <- fileList[grep(".xml", tolower(fileList), fixed = TRUE)]
   if(length(xmlFiles) == 0) stop("Something is wrong - no xml")
   # should make not enough look for specific files

   # load xml into list
   xmlContents <- vector(mode = "character", length = length(xmlFiles))
   for(i in seq(along = xmlContents)) {
      xmlContents[i] <- readXML(xmlFiles[i], verbose = control$verbose) }
   names(xmlContents) <- xmlFiles

   # add style information, if any, to styles.xml and content.xml
   styles <- getStyleDefs()
   styleInfo <- xmlContents["styles.xml"]
   xmlContents["styles.xml"] <- addStyleDefs(styleInfo, styles, "styles", control$verbose)

   styleInfo <- xmlContents["content.xml"]
   xmlContents["content.xml"] <- addStyleDefs(styleInfo, styles, "content", control$verbose)

   #get indexes and lengths of sweave expression matches
   stags <- tagsIdxs(xmlContents)
   #for convenience
   tagsFound <- tagsExist(stags)

   if(any(tagsFound))
   {
      announce("\n  Sweave tags found in: ", xmlFiles[tagsFound], "\n")
      sweaveFiles <- xmlFiles[tagsFound]
      sweaveContents <- xmlContents[tagsFound]

      for(j in seq(along = sweaveFiles))
      {
         announce("  Removing xml around <<>>= for ", sweaveFiles[j], "\n")
         if(!is.null(sweaveContents[j])) sweaveContents[j] <-
         processXml(sweaveContents[j], stags[,j])
         # write processed lines to Rnw file
         announce("  Writing ", sweaveFiles[j], " to ", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n")
         rnwFile <- file(paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw",
            sweaveFiles[j]), sep = ""), "wb")
         writeXML(sweaveContents[[j]], rnwFile)

         #nuke the xml file
         announce("\n  Removing ", sweaveFiles[j], "\n")
         if(unlink(paste(workDir, "/", sweaveFiles[j], sep = ""), recursive = TRUE) == 1)
            stop("Error removing xml file file")
         announce("  Sweaving ",gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n\n")
         #not used in current implementation
         #Sys.setlocale("LC_CTYPE", currentLocale[1])
         #Sys.setlocale("LC_COLLATE", currentLocale[2])

         #Sweave results to new xml file
         Sweave(
            file =   paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""),
            output = paste(workDir, "/", sweaveFiles[j], sep = ""),
            quiet = !control$verbose,
            driver = RweaveOdf(), control = control)

         #not used in current implementation
         #Sys.setlocale("LC_CTYPE", "C")
         #Sys.setlocale("LC_COLLATE", "C")

         # remove sweave file
         announce("  Removing ", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), "\n")
         if(unlink(paste(workDir, "/", gsub("[Xx][Mm][Ll]", "Rnw", sweaveFiles[j]), sep = ""), recursive = TRUE) == 1)
            stop("Error removing xml file file")
         }
   }

   # if there was no Sweave tags in styles.xml, write that out too
   if(!tagsFound[which(xmlFiles == "styles.xml")])
   {
      styleFile <- file(paste(workDir, "/styles.xml", sep = ""), "wb")
      sink(styleFile)
      cat(xmlContents[[which(xmlFiles == "styles.xml")]])
      sink()
      close(styleFile)
   }

   zipCmd[1] <- gsub(
      "$$file$$",
      paste(
         ifelse(.Platform$OS.type == "windows", "\"", ""),
         workDir,
         "/",
         basename(file),
         ifelse(.Platform$OS.type == "windows", "\"", ""),
         sep = ""),
      zipCmd[1],
      fixed = TRUE)

   announce("\n\  Packaging file using", zipCmd[1], "\n")
   if(.Platform$OS.type == "windows")
   {
      if(system(zipCmd[1], invisible = TRUE) != 0)  stop("Error zipping file")
   } else {
      if(system(zipCmd[1]) != 0) stop("Error zipping file")
   }

   # copy final file to destination
   if(!file.exists(workingCopy))  stop(paste(workingCopy, "does not exist"))
   announce("  Copying ", workingCopy, "\n")
   if(!file.copy(workingCopy, dest, overwrite = TRUE))  stop("Error copying odt file")

   announce("  Resetting wd\n")
   setwd(currentLoc)

   #Sys.setlocale("LC_CTYPE", currentLocale[1])
   #Sys.setlocale("LC_COLLATE", currentLocale[2])

   # delete working dir
   if(control$cleanup)
   {
      announce("  Removing ", workDir, "\n")
      if(unlink(workDir, recursive = TRUE) == 1) stop("Error removing work dir")
   }
   invisible(NULL)
}

announce <- function (...) {
   control=odfWeaveControl()
   if (control$verbose) cat(...)
   flush.console()
}

"attR<-" <- function (x, name, value) {
   #apply an attribute lists in a list
   #value always gets unlisted before assignment
   if( (length(value) != length(x)) || (length(value) == 1))
      lapply(x, function(x) {attr(x, name) <- value[[1]]; x})
   else
      lapply(seq(x), function(y) { attr(x[[y]], name) <- value[[y]]; x[[y]]})
}

"attR" <- function (x, name) {
   #return the values of attribute "name" items in list
   lapply(x, function(x) attr(x, name))
}

"getByAttR" <- function (x, name, value) {
 #return items in list x with attribute called "name"
 x[unlist(lapply(x, function(y) attr(y, name) == value))]
}

"debugWrite" <- function(filename, content){
   #used at breakpoints during debugging
   outFile <- file(filename, "wb")
   writeBin(content, outFile)
   close(outFile)
}

"subin" <- function(x, matches, pieces){
   piece <- length(pieces)
   if (piece == 0) return(x)
   matchstart <- matches[[piece]]
   matchend <- matchstart + attr(matches, "match.length")[piece]
   part1 <- substr(x, 1, matchstart - 1)
   part2 <- substr(x, matchstart, matchend -1)
   part3 <- substr(x, matchend, nchar(x))
   paste(
      c(
         subin(
            part1,
            matches,
            pieces[-piece]),
         pieces[piece],
         part3),
      collapse = "")
}

"tagsIdxs" <- function(x) {
      #input:  character
      #output: 2-way integer list
      #   match offsets for each character/pattern

      #TODO:  add names to list instead of to "matchtype" attribute

      matchtype = "match.type"

      out1 <- gregexpr("(?s)\\\\Sexpr\\{[^\\}]*?\\}", x, perl=TRUE)
      attR(out1, matchtype) <- "sexpr"
      out2 <- gregexpr("(?s)(?U)(<text:p)(?(?=text:p)(?!)|.)*&lt;&lt;.*&gt;&gt;=.*>@<(/text:p>|.*</text:p>)", x, perl=TRUE)
      attR(out2, matchtype) <- "chunk"
      out3 <- gregexpr("(?s)\\\\SweaveOpts\\{[^\\}]*?\\}", x, perl=TRUE)
      attR(out3, matchtype) <- "option"
      mapply(list, out1, out2, out3)
      }

"tagsExist" <- function(x) {
   #input:  single list or list of lists
   #output:  boolean
   tagsExist_ <- function(y) any(attr(y, "match.length") != -1)
   if (is.null(dims <- dim(x))) return(tagsExist_(x))
   x <- unlist(lapply(x, tagsExist_))
   dim(x) <- dims
   apply(x, 2, function(y) any(y)) }


"tagsGet" <- function (x, matches) {
   #input:
   #
   #   x: character
   #
   #   matches:  list of integers with atribute 'match.length'
   #
   #output: character
   unlist(
      lapply(
         seq(length(matches)),
         function (y) {
            len <- attr(matches, "match.length")[y]
            if (len < 0) return(-1)
            substr(x, matches[y],
               matches[y] + attr(matches, 'match.length')[y] - 1)
         }
      )
   )
}

"writeXML" <- function(content, outFile, method="old", verbose=FALSE){
   writeBin(content, outFile)
   close(outFile)
}
