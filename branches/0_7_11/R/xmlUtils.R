#
# This file contains utility functions that are used for
# generating XML, and used from preproc.R, postproc.R,
# procstyles.R, and possibly others.
#
# None of these functions are exported by the odfWeave package.

verbose <- FALSE

debug <- function(...)
{
   if (verbose)
   {
      write(paste(list(...), collapse=' '), stderr())
      flush.console()
   }
}

# remove whitespace from beginning and end of input string
trim <- function(x)
{
   sub('[[:space:]]+$', '', sub('^[[:space:]]+', '', x))
}

# escape characters by replacing with XML entities
escape <- function(x)
{
   x <- gsub('&', '&amp;',  x, fixed=TRUE)
   x <- gsub('<', '&lt;',   x, fixed=TRUE)
   x <- gsub('>', '&gt;',   x, fixed=TRUE)
   x <- gsub("'", '&apos;', x, fixed=TRUE)
   x <- gsub('"', '&quot;', x, fixed=TRUE)
   x
}

correct <- function(x)
{
   x <- gsub('\342\200\223', '-', x)
   x <- gsub('\342\200\235', '"', x)
   x <- gsub('\342\200\234', '"', x)
   x
}

# convert a hex string to a decimal number
hex2dec <- function(h)
{
   hdigits <- c("0", "1", "2", "3", "4", "5", "6", "7",
                "8", "9", "a", "b", "c", "d", "e", "f")
   val <- 0
   for (d in digits <- strsplit(tolower(h), "")[[1]])
   {
      i <- which(d == hdigits)
      if (length(i) == 0)
         stop("illegal hex string passed to hex2dec")
      val <- val * 16 + i - 1
   }
   val
}

# convert a hex string to an ascii character
hex2ascii <- function(h)
{
   # ascii values from 32 to 126
   ascii <- c(' ', '!', '"', '#', '$', '%', '&', "'", '(', ')', '*',
              '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5',
              '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@',
              'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
              'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
              'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_', '`', 'a',
              'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
              'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
              'x', 'y', 'z', '{', '|', '}', '~')
   d <- hex2dec(h)
   if (d < 32 || d > 126) '#' else ascii[d - 31]
}

# convert a string with ascii escape sequences into normal ascii
# for example, convert "%5cSexpr%7b%7d" to "\Sexpr{}"
unquote <- function(expr)
{
   matches <- gregexpr("%[0-9A-Fa-f][0-9A-Fa-f]", expr)

   if (matches[[1]][1] != -1)
   {
      nexpr <- c()
      i <- 1
      for (match in matches[[1]])
      {
         nexpr <- c(nexpr, substr(expr, i, match - 1))
         i <- match + 3  # the length of the match is always 3
         # extract the match, without the '%'
         nexpr <- c(nexpr, hex2ascii(substr(expr, match + 1, i - 1)))
      }
      nexpr <- c(nexpr, substr(expr, i, nchar(expr)))
      expr <- paste(nexpr, collapse='')
   }
   expr
}

# process the values of the specified XML attributes, unquoting
# Sexpr's so they're ready for Sweave
processAttributes <- function(atts)
{
   unlist(lapply(atts, function(att)
   {
      matches <- gregexpr('(\\\\Sexpr\\{[^{}]*\\}|%5[Cc]Sexpr%7[Bb].*?%7[Dd])', att, perl=TRUE)

      # if there's no matches, do nothing at all
      if (matches[[1]][1] != -1)
      {
         natt <- c()
         i <- 1
         matlens <- attr(matches[[1]], 'match.length')
         for (j in seq(along.with=matches[[1]]))
         {
            match <- matches[[1]][j]
            natt <- c(natt, escape(substr(att, i, match - 1)))
            i <- match + matlens[j]
            natt <- c(natt, correct(unquote(substr(att, match, i - 1))))
         }

         natt <- c(natt, escape(substr(att, i, nchar(att))))
         att <- paste(natt, collapse='')
      } else {
         att <- escape(att)
      }
      att
   }))
}

# this function generates a string for the attributes
# of an XML tag.
genXMLAttributes <- function(atts)
{
   if (length(atts) > 0)
      paste(' ', names(atts), '="', processAttributes(atts), '"', sep='', collapse='')
   else
      ''
}

startTag <- function(name, atts, outfile)
{
   cat('<', name, genXMLAttributes(atts), sep='', file=outfile)
}

openTag <- function(name, atts, outfile)
{
   startTag(name, atts, outfile)
   cat('>', file=outfile)
}

closeTag <- function(name, outfile)
{
   cat(sprintf('</%s>', name), file=outfile)
}

completeTag <- function(name, atts, outfile)
{
   startTag(name, atts, outfile)
   cat('/>', file=outfile)
}

processText <- function(t, outfile)
{
   if (!is.null(t))
      cat(escape(t), file=outfile)
}
