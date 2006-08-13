odfWeaveControl <- function(
   zipCmd = c("zip -r $$file$$ .", "unzip -o $$file$$"),
   cleanup = FALSE,
   verbose = TRUE)
{
	# pass ... args to figGen here?
   list(
      zipCmd = zipCmd,
      cleanup = cleanup,
      verbose = verbose)
}

