"odfStyleGen" <-
function(x)
{
   out <- vector(mode = "character", length = 0)
   
   if(!is.null(x$paraText))
   {
      paraFormat <- paste(
         "\n  <style:style style:name=\"",
         x$paraText$name,
         "\" style:family=\"paragraph\"",
         " style:parent-style-name=\"",
         x$paraText$parentStyleName,
         "\">\n",
         "   <style:text-properties ", 
         ifelse(!is.null(x$paraText$textAlign),
            paste(" fo:text-align=\"", x$paraText$textAlign,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$paraText$fontColor),
            paste(" fo:color=\"", x$paraText$fontColor,"\"", sep = ""),
            ""),         
         ifelse(!is.null(x$paraText$fontName),
            paste(" style:font-name=\"", x$paraText$fontName,"\"", sep = ""),
            ""),          
         ifelse(!is.null(x$paraText$fontWeight),
            paste(" fo:font-weight=\"", x$paraText$fontWeight,"\"", sep = ""),
            ""), 
         "/>\n  </style:style>\n",         
         sep = "")      
      out <- c(out, paraFormat)
   }  
     
   if(!is.null(x$cellText))
   {
   
      cellTextFormat <- paste(
         "\n  <style:style style:name=\"",
         x$cellText$name,
         "\" style:family=\"paragraph\"",
         " style:parent-style-name=\"",
         x$cellText$parentStyleName,
         "\">\n",
         "   <style:paragraph-properties ", 
         ifelse(!is.null(x$cellText$textAlign),
            paste(" fo:text-align=\"", x$cellText$textAlign,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$cellText$justifySingleWord),
            paste(" style:justify-single-word=\"", x$cellText$justifySingleWord,"\"", sep = ""),
            ""),    
         " />\n   <style:text-properties ",
         ifelse(!is.null(x$cellText$fontColor),
            paste(" fo:color=\"", x$cellText$fontColor,"\"", sep = ""),
            ""),         
         ifelse(!is.null(x$cellText$fontName),
            paste(" style:font-name=\"", x$cellText$fontName,"\"", sep = ""),
            ""),          
         ifelse(!is.null(x$cellText$fontWeight),
            paste(" fo:font-weight=\"", x$cellText$fontWeight,"\"", sep = ""),
            ""),           
         "/>\n  </style:style>\n",         
         sep = "")      
      out <- c(out, cellTextFormat)
   } 
   
   if(!is.null(x$cellHeaderText))
   {
   
      cellHeaderTextFormat <- paste(
         "\n  <style:style style:name=\"",
         x$cellHeaderText$name,
         "\" style:family=\"paragraph\"",
         " style:parent-style-name=\"",
         x$cellHeaderText$parentStyleName,
         "\">\n",
         "   <style:paragraph-properties ", 
         ifelse(!is.null(x$cellHeaderText$textAlign),
            paste(" fo:text-align=\"", x$cellHeaderText$textAlign,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$cellHeaderText$justifySingleWord),
            paste(" style:justify-single-word=\"", x$cellHeaderText$justifySingleWord,"\"", sep = ""),
            ""),    
         " />\n   <style:text-properties ",
         ifelse(!is.null(x$cellHeaderText$fontColor),
            paste(" fo:color=\"", x$cellHeaderText$fontColor,"\"", sep = ""),
            ""),         
         ifelse(!is.null(x$cellHeaderText$fontName),
            paste(" style:font-name=\"", x$cellHeaderText$fontName,"\"", sep = ""),
            ""),          
         ifelse(!is.null(x$cellHeaderText$fontWeight),
            paste(" fo:font-weight=\"", x$cellHeaderText$fontWeight,"\"", sep = ""),
            ""),           
         "/>\n  </style:style>\n",         
         sep = "")      
      out <- c(out, cellHeaderTextFormat)
   }    
   
   if(!is.null(x$cell))
   {
      cellFormat <- paste(
         "\n  <style:style style:name=\"",
         x$cell$name,
         "\" style:family=\"table-cell\">\n   <style:table-cell-properties ", 
         ifelse(!is.null(x$cell$marginLeft),
            paste(" fo:background-color=\"", x$cell$marginLeft,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$cell$padding),
            paste(" fo:padding=\"", x$cell$padding,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$cell$leftBorder),
            paste(" fo:border-left=\"", x$cell$leftBorder,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$cell$rightBorder),
            paste(" fo:border-right=\"", x$cell$rightBorder,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$cell$topBorder),
            paste(" fo:border-top=\"", x$cell$topBorder,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$cell$bottomBorder),
            paste(" fo:border-bottom=\"", x$cell$bottomBorder,"\"", sep = ""),
            ""),              
         " >\n   </style:table-cell-properties>\n  </style:style>\n",
         sep = "")   
      out <- c(out, cellFormat)
                       
   } 
  
   if(!is.null(x$column))
   {
      columnFormat <- paste(
         "\n  <style:style style:name=\"",
         x$column$name,
         "\" style:family=\"table-column\">\n   <style:table-column-properties ", 
         ifelse(!is.null(x$column$width),
            paste(" column-width=\"", x$column$width,"\"", sep = ""),
            ""),
         " />\n  </style:style>\n",
         sep = "")   
      out <- c(out, columnFormat)    
   } 
   
   if(!is.null(x$table))
   {
      
      tableFormat <- paste(
         "\n  <style:style style:name=\"",
         x$table$name,
         "\" style:family=\"table\">\n   <style:table-properties ", 
         ifelse(!is.null(x$table$marginLeft),
            paste(" fo:margin-left=\"", x$table$marginLeft,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$table$marginRight),
            paste(" fo:margin-right=\"", x$table$marginLeft,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$table$marginTop),
            paste(" fo:margin-top=\"", x$table$marginTop,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$table$marginBottom),
            paste(" fo:margin-bottom=\"", x$table$marginBottom,"\"", sep = ""),
            ""), 
         " />\n  </style:style>\n",
         sep = "")
      out <- c(out, tableFormat)    
      
   } 
   
   if(!is.null(x$bulletList))
   {
      
      bulletListFormat <- paste(
         "\n  <text:list-style style:name=\"",
         x$bulletList$name,
         "\">\n   <text:list-level-style-bullet ",
         ifelse(!is.null(x$bulletList$level),
            paste(" text:level=\"", x$bulletList$level,"\"", sep = ""),
            ""),         
         ifelse(!is.null(x$bulletList$styleName),
            paste(" text:style-name=\"", x$bulletList$styleName,"\"", sep = ""),
            ""),                     
         ifelse(!is.null(x$bulletList$bulletChar),
            paste(" text:bullet-char=\"", x$bulletList$bulletChar,"\"", sep = ""),
            ""),
         ">\n    <style:list-level-properties ",
         
         ifelse(!is.null(x$bulletList$spaceBefore),
            paste(" text:space-before=\"", x$bulletList$spaceBefore,"\"", sep = ""),
            ""),
         ifelse(!is.null(x$bulletList$minLabelWidth),
            paste(" text:min-label-width=\"", x$bulletList$minLabelWidth,"\"", sep = ""),
            ""),
         "/>\n    <style:text-properties ",
         ifelse(!is.null(x$bulletList$fontName),
            paste(" style:font-name=\"", x$bulletList$fontName,"\"", sep = ""),
            ""), 
         " />\n   </text:list-level-style-bullet>\n  </text:list-style>\n",
         sep = "")
      out <- c(out, bulletListFormat)    
      
   }    
   out <- c(out, "\n")
   
   
   out
}

