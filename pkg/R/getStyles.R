
setImageDefs <- function(def, verbose = TRUE)
{
   if(!(all(names(defs) %in% c("type", "device", "plotHeight", 
      "plotWidth", "dispHeight", "dispWidth", "args" ))))
      stop("arguments were included. see ?setImageDefs")

   if(!is.null(def$args))
   {
      if(!is.list(def$args)) stop("args must be a list")
      if(any(names(def$args) %in% c("width", "height")))
         stop("these options should be specified without using the args list")
   }
   
  
   if(def$device %in% c("pdf", "svg")) stop("pdf and svg formats not supported by OpenOffice")   
   if(def$device %in% c("png", "bmp", "jpeg"))
   {
      if(def$device == "png" & !capabilities("png"))
         stop("R png device not enabled")
      if(def$device %in% c("bmp", "jpeg") & !capabilities("jpeg"))
         stop(paste("R", def$device,"device not enabled"))      
   }
   
  
   if(def$device == "postscript" & verbose)
   {
      psArgs <- names(def$args) %in% c("horizontal", "onefile", "paper")
      psNote <- paste(
         "you will probabiliy need to set",
         "\nhorizontal = FALSE, onefile = FALSE,",
         "and paper = \"special\" to",
         "generate ps graphics for OpenOffice\n")
      if(length(psArgs) == 0 | any(!psArgs)) cat(psNote)
   }
   current <- getImageDefs()
   for(i in names(current))
   {
      if(!is.null(def[[i]]) && current[[i]] != def[[i]]) current[[i]] <- def[[i]]
   }
   
   if(current$device %in% c("png", "bmp", "jpeg") & verbose)
   {
      if(current$plotHeight <= 30 | current$plotWidth <= 30)
         cat(
            paste(
               "an image size of",
               current$plotHeight,
               "pixels by",
               current$plotWidth,
               "pixels has been requested.\n"))   
   } else {
      if(current$plotHeight >= 30 | current$plotWidth >= 30)
         cat(
            paste(
               "an image size of",
               current$plotHeight,
               "inches by",
               current$plotWidth,
               "inches has been requested.\n"))     
   }
   
   flush.console()
   assign("imageDefs",  current, env = .odfEnv)
}

