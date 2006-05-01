"odfTable" <-
function(x, ...)
{
   UseMethod("odfTable")
}

print.odfTable <- function(x, ...) cat(x)

