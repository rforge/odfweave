odfWeaveControl <- function(
   zipCmd = c("zip -r $$file$$ .  -i *", "unzip -o $$file$$"),
   cleanup = FALSE,
   style = defaultStyles,
   verbose = TRUE,
   plotType = "png",
   plotDevice = ifelse(.Platform$OS.type == "windows", "png", "bitmap"),
   figHeight = 5,
   figWidth = 5)
{
	# pass ... args to figGen here?
   list(
      zipCmd = zipCmd,
      cleanup = cleanup,
      style = style,
      verbose = verbose,
      plotType = plotType,
      plotDevice = plotDevice,
      figHeight = figHeight,
      figWidth = figWidth)
}

