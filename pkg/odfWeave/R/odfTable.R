#Zekai Otles added cgroup,rgroup,n.cgroup,n.rgroup similar ideas to F. Harrell Misc
#package, see Hmisc latex package
"odfTable" <-
function(x, ...)
{
   UseMethod("odfTable")
}

print.odfTable <- function(x, ...)
{
   cat(x$start)
   if(!is.null(x$header)) cat(x$header)
   if(any(dim(x$cells) == 1)) cat(x$cells) else cat(t(x$cells))
   cat(x$end)
}

