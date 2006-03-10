"odfList" <-
function(data, styles = NULL)
{
   if(!is.vector(data)) stop("data must be a vector")
   if(!is.null(styles$bulletList)) listStyle <- paste(" text:style-name=\"", styles$bulletList, "\" ", sep = "")
      else listStyle <- ""
   if(!is.null(styles$paraText)) textStyle <- paste(" text:style-name=\"", styles$paraText, "\" ", sep = "")
      else textStyle <- ""      
      
   x <- paste(
      "    <text:list-item>\n",
      "      <text:p ",
      textStyle,
      ">",
      data,
      "</text:p>\n",      
      "    </text:list-item>\n",
      sep = "")

   paste(
      "\n  <text:list ",
      listStyle,
      ">\n",
      paste(x, collapse = ""),
      "  </text:list>\n",
      sep = "")
}

