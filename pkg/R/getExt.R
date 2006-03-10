"getExt" <-
function(x)
{
   hasPeriod <-  grep("\\.", x)
   if(length(x) == 1 & length(hasPeriod) == 0)
   {
      z <- NA
   } else {
      if(length(hasPeriod) < length(x)) x[-hasPeriod] <- NA
      y <- strsplit(x, ".", extend = FALSE, fixed = TRUE)
      z <- lapply(y, function(data) data[length(data)])         
   }
   unlist(z)
}



