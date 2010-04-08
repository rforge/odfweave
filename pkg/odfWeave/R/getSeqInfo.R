# This function is used to initialize the "seqInfo" object in the
# .odfEnv environment.  This must be done before the Sweave phase.
# It used to be done as part of the preprocessing phase.  We could
# continue to do then, but it isn't logically part of preprocessing.
# It was just convenient, and perhaps confusing, to do it then.

# XXX We should consider restructuring the code so that we pass
# in the document object, rather than parsing the input file once
# during preprocessing, and once here, also.
getSeqInfo <- function(infile)
{
   doc <- xmlTreeParse(infile, trim=FALSE, addAttributeNamespaces=TRUE)
   node <- doc$doc$children[[1]]
   sfun <- function(s) xmlGetAttr(s, 'text:ref-name')
   refnames <- unlist(treeapply(node, 'text:sequence', sfun, onlyFirst=FALSE, rooted=FALSE))
   list(Table=refnames, Illustration=refnames)
}
