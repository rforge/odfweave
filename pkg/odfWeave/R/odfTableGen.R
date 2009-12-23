"odfTableGen" <-
function(x, dataType, header = NULL, tableName, styles,cgroup=NULL,n.cgroup=NULL,rgroup=NULL,n.rgroup=NULL)
{

   tableDim <- dim(x)
#If there first column has information update styles

  if(!is.null(rgroup)){
	  tableDim[2]<-tableDim[2]+1
	  x<-cbind("",x)
	  dataType<-c("string",dataType)
       	  header<-c("",header)
	  names(header)[1]<-"V1"
	  styles <- tableStyles(x, useRowNames = FALSE, dimnames(x)[[2]])
  }


   has <- function(x) !is.null(x) && x != ""
   makeMatrix <- function(text, dims) matrix(rep(text,prod(dims)),nrow = dims[1])

   endQuote <- makeMatrix("\" ", dim(styles$cell))


   # build text tags and formatting
   textNameStart <- makeMatrix(" text:style-name=\"", dim(styles$text))
   textNameStart <- ifelse(styles$text == "", "", textNameStart)
   textNameEnd <- ifelse(styles$text == "", "", endQuote)
   textName <- matrixPaste(textNameStart, styles$text, textNameEnd, sep = c("", ""))
   textStart <- makeMatrix("      <text:p", dim(styles$text))
   textEnd <- makeMatrix(">", dim(styles$text))
   tagEnd <- makeMatrix("</text:p>\n", dim(styles$text))
   textMatrix <- matrixPaste(textStart, textName, textEnd, x, tagEnd,
                             sep = rep("", 5))

   # cell properties
   cellNameStart <- makeMatrix(" table:style-name=\"", dim(styles$cell))
   cellNameStart <- ifelse(styles$cell == "", "", cellNameStart)
   cellNameEnd <- ifelse(styles$cell == "", "", endQuote)
   cellName <- matrixPaste(cellNameStart, styles$cell, cellNameEnd, sep = c("", ""))
   valueType <- matrix(rep(dataType, each = tableDim[1]), nrow = tableDim[1])
   valueTypeStart <- makeMatrix(" <table:table-cell office:value-type=\"", dim(styles$cell))
   cellStart <- matrixPaste(valueTypeStart, valueType, endQuote, sep = c("", ""))
   cellEnd <- makeMatrix(">\n", dim(styles$cell))
   tagEnd <- makeMatrix(" </table:table-cell>\n", dim(styles$cell))
   cellMatrix <- matrixPaste(cellStart, cellName, cellEnd, textMatrix, tagEnd)

   #try this to add spanned rows
      firstCol<-NULL
	if(!is.null(rgroup)){
		numHeadSpawn<-length(rgroup)
		firstCol0<-array("",dim=tableDim[1])
		aIndx<-1
		for(indx in 1:numHeadSpawn){
			if(indx > 1){aIndx<-1+sum(n.rgroup[1:indx-1])}
			firstCol0[aIndx]<- paste("<table:table-cell table:style-name=", '"',
				"noBorder",'" ',
			paste("table:number-rows-spanned=",'"',n.rgroup[indx],'"',collapse='"'),
			" office:value-type=\"string\">",
			paste("\n      <text:p ", "text:style-name=",'"',"ArialCenteredBold",'"> ',sep=""),
				rgroup[indx], "</text:p>",
			"\n    </table:table-cell>", sep = "")
		}
	firstCol<- paste(firstCol0,collapse="")
	}

#try this
	rowTagsOffSet<-1
	#Following adjustment made for the If there is prescriped first Col for header
	if(!is.null(firstCol)){
		cellMatrix[,1]<-firstCol0
			cgroup<-c("",cgroup)
			n.cgroup<-c(1,n.cgroup)
	}#otherwise keep cellMatrix as it is

   # wrap in row tags
   leftRowTags <- matrix(
      c(
         rep("<table:table-row>", tableDim[1]),
         rep("", (tableDim[2] - rowTagsOffSet) * tableDim[1])),
      nrow = tableDim[1])
   rightRowTags <- matrix(
      c(
         rep("", (tableDim[2] - rowTagsOffSet) * tableDim[1]),
         rep("</table:table-row>\n", tableDim[1])),
      nrow = tableDim[1])
   rowMarkup <- matrixPaste(leftRowTags, cellMatrix, rightRowTags, sep = rep("\n", 2))

   if(has(styles$table)) tableStyle <- paste(" table:style-name=\"", styles$table, "\" ", sep = "")
   else tableStyle <- ""

   if(!is.null(header))
   {
      cellHeaderStyle <- paste(" table:style-name=\"", styles$headerCell, "\" ", sep = "")
      cellHeaderStyle <- ifelse(styles$headerCell == "", "", cellHeaderStyle)


      textHeaderStyle <- paste(" text:style-name=\"", styles$header, "\" ", sep = "")
      textHeaderStyle <- ifelse(styles$header == "", "", textHeaderStyle)

      firstRow<-NULL
	if(!is.null(cgroup)){
		numHeadCol<-length(cgroup)

		firstRow0<- paste("\n      <text:p ", textHeaderStyle[1:numHeadCol], ">", cgroup, "</text:p>", sep = "")
		firstRow01<-array("",dim=numHeadCol)
		for(indx in 1:numHeadCol){
			firstRow01[indx]<- paste("\n    <table:table-cell ", cellHeaderStyle[indx],
			paste("table:number-columns-spanned=",'"',n.cgroup[indx],'"',collapse='"'),
			" office:value-type=\"string\">",
			firstRow0[indx],"\n    </table:table-cell>", sep = "")
		}
	firstRow<- paste(paste("<table:table-row>",paste(firstRow01,collapse=""),sep=""),"\n    </table:table-row>\n", sep="")
	}


      headLine01 <- paste("\n      <text:p ", textHeaderStyle, ">", header, "</text:p>", sep = "")
      headLine02 <- paste(
         "\n    <table:table-cell ",
         cellHeaderStyle,
         "office:value-type=\"string\">",
         headLine01,
         "\n    </table:table-cell>",
         sep = "")



      headLine03 <- paste(
         "\n  <table:table-header-rows>\n",firstRow,
	 paste("<table:table-row>\n",
		 paste(headLine02,collapse=""),
         "\n    </table:table-row>\n",
		 sep=""),
	 "</table:table-header-rows>\n",
         sep = "")

   } else headLine03 <-  NULL


   startText <- paste(
      "\n<table:table table:name=\"",  tableName, "\" ", tableStyle, ">",
      "\n  <table:table-column ",
      "table:number-columns-repeated=\"", length(dataType), "\"/>",
      sep = "")

   list(
      start = startText,
      header = headLine03,
      cells = rowMarkup,
      end = "\n</table:table>\n")
}
