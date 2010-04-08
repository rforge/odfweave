initStyleNames <- function(infile, env)
{
   doc <- xmlTreeParse(infile, trim=FALSE, addAttributeNamespaces=TRUE)
   node <- doc$doc$children[[1]]
   sfun <- function(s) xmlGetAttr(s, 'style:name', 'ERROR')
   stylenames <- unlist(treeapply(node, 'style:style', sfun, onlyFirst=FALSE, rooted=FALSE))

   for (sname in stylenames)
   {
      assign(sname, sname, pos=env)
   }
}
