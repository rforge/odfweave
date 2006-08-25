control <- odfWeaveControl()
verbose <- control$verbose

"addStyleDefs" <- function(x, style, dest = "styles", verbose = TRUE) {
   #input
   #
   #   x
   #      single-element character vector, the contents of a document
   #
   #value: x with inserted styles


   # add font declarations
   x <- addFontDecs(x)

   # we need to add our style info between <office:styles> tags
   # which may not exist
   startTag <- if(dest == "styles") "<office:styles>" else "<office:automatic-styles>"
   emptyTag <- sub(">", "/>", startTag)
   endTag <- sub("<", "</", startTag)
   if(verbose) cat(paste("\n  Looking for style information in " , dest, ".xml\n", sep = "")); flush.console()
   emptyTagIndex <- regexpr(emptyTag, x)
   if(emptyTagIndex > 0 )
   {
      if (emptyTagIndex == 1) {
         part1=""
      } else {
         part1 <- substr(x, 1, emptyTagIndex -1)
      }
      part2 <- substr(x, emptyTagIndex, nchar(x))
      x <- paste(c(part1, startTag, endTag, part2), collapse="")
   }
   locateAutoStart <- gregexpr(startTag, x, fixed = TRUE)[[1]]
   locateAutoStop <- gregexpr(endTag, x, fixed = TRUE)[[1]]
   if(length(locateAutoStart) > 1 & length(locateAutoStop) > 1)
      stop(paste("there are more that one ", startTag, "tags"))

   if(verbose) cat(paste("  Splitting file around", startTag, "at line", locateAutoStop, "\n")); flush.console()
   part1 <- substr(x, 1, locateAutoStop - 1) # xml before tag
   part2 <- substr(x, locateAutoStop, nchar(x)) # xml after tag
   newstyle <- odfStyleGen(style, type = dest)
   if(verbose) cat("\n")
   out <- paste(c(part1, newstyle, part2), collapse="")
   out
}

"addFontDecs" <- function (content) {
   #input
   #
   #   content:  character string, contents of a document
   #
   #value: character string, x with additional font declarations appended.
   #    Fonts listed in odfEnv.R are checked and added, if necessary
   #

   announce(verbose, "adding font declarations...\n")

   #get styles used in odfEnv
   styles <- getStyleDefs()
   #extract font names from styles
   fonts <- unique(unlist(lapply(styles, function(font) font$fontName)))

   #get font declarations
   #assert:  content only has one such font-face-decls element
   match <- regexpr(
      "(?s)(?U)<office:font-face-decls>.*</office:font-face-decls>",
      content, perl=TRUE)
   matchLength <- attr(match, "match.length")
   fontDecBlock <- substr(content, match, match + matchLength -1)

   #get only fonts which don't have a declaration
   fonts <-fonts[!unlist(lapply(lapply(fonts, grep, fontDecBlock), any))]

   #create a string of font declarations to insert
   fontDecs <- paste(
      unlist(
         lapply(fonts,
            function(font){
               element(
                  "style:font-face",
                  c(
                     tagattr("style:name", font),
                     tagattr("svg:font-family", font)
                  )
               )
            }
         )
      ),
      collapse=""
   )
   #insert font declarations
   sub("</office:font-face-decls>",
      paste(fontDecs, "</office:font-face-decls>", sep=""),
      content,
      fixed=TRUE
   )
}

