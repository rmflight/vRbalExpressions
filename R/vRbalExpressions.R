#' Construct a verbal expression
#' 
#' Take a series of named arguments, and return the actual verbal expression for use with R's various regular expression functions
#' 
#' @export
#' @return character string with the constructed regular expression
#' @examples
#' testExpr <- verbalExpression(startofline=NULL, then="http",maybe="s",then="://",maybe="www.",anythingBut=" ",endofline=NULL)
#' testWWW <- "https://www.google.com"
#' grepl(testExpr, testWWW)
#' 
#' testExpr <- verbalExpression(find="bird")
#' sub(testExpr, "duck", "Replace bird with duck")
#' @rdname verEx
verbalExpression <- function(...){
  inArgs <- list(...)
  
  validFunctions <- ls("package:vRbalExpressions")
  itemNames <- names(inArgs)
  validItem <- itemNames %in% validFunctions
  
  inValid <- which(!validItem)
  
  if (length(inValid) != 0){
    for (iI in inValid){
      warnMessage <- paste(itemNames[iI], " is not a valid verbalExpression function, it was dropped!", sep="", collapse="")
      warning(warnMessage, call.=FALSE)
    }
    inArgs <- inArgs[validItem]
    itemNames <- itemNames[validItem]
  }
  
  nItem <- length(inArgs)
  
  outStr <- vector("list", nItem)
  for (iItem in 1:nItem){
    inData <- inArgs[[iItem]]
    outStr[[iItem]] <- eval(call(itemNames[iItem], inData))
  }
  return(paste0(unlist(outStr), collapse=""))
}

#' look for something at the beginning of a line
#' 
#' @export
#' @rdname verEx
startofline <- function(value){
  if (is.null(value)){
    outval <- "^"
  } else {
    outval <- paste0("^", value)
  }
  return(outval)
}

#' look for something at the end of a line
#' @export
#' @rdname verEx
endofline <- function(value){
  if (is.null(value)){
    outVal <- "$"
  } else {
    outVal <- paste0(value, "$")
  }
  return(outVal)
}

#' what to look for
#' @export
#' @rdname verEx
then <- function(value){
  return(paste0("(?:", value, ")"))
}

#' alias for `then` for start of an expression
#' 
#' @rdname verEx
find <- function(value){
  then(value)
}

#' maybe something is there?
#' 
#' @export
#' @rdname verEx
maybe <- function(value){
  return(paste0("(?:", value, ")?"))
}

#' any character any number of times
#' @export
#' @rdname verEx
anything <- function(value){
  return("(?:.*)")
}

#' any character any number of times BUT this
#' @export
#' @rdname verEx
anythingBut <- function(value){
  return(paste0("(?:[^", value , "]*)"))
}

#' Any character at least one time
#' @export
#' @rdname verEx
something <- function(value){
  return("(?:.+)")
}

#' Any character at least one time except for these characters
#' @export
#' @rdname verEx
somethingBut <- function(value){
  return(paste0("(?:[^", value, "]+)"))
}

#' Line break
#' @export
#' @rdname verEx
linebreak <- function(value){
  return("(?:(?:\\n)|(?:\\r\\n))")
}

#' html shorthand
#' @export
#' @rdname verEx
br <- function(value){
  linebreak(NULL)
}

#' tab
#' @export
#' @rdname verEx
tab <- function(value){
  return("\\t")
}

#' Any alphanumeric
#' @export
#' @rdname verEx
word <- function(value){
  return("\\w+")
}

#' Any given character
#' @export
#' @rdname verEx
anyOf <- function(value){
  return(paste0("[", value, "]"))
}

#' Any given character (short)
#' @export
#' @rdname verEx
any <- function(value){
  anyOf(value)
}

#' Range
#' @export
#' @rdname verEx
range <- function(from, to){
  return(paste0("[", from, "-", to, "]"))
}

#' or
#' @export
#' @rdname verEx
or <- function(value){
  tmpVal <- then(value)
  return(paste0("|", tmpVal))
}