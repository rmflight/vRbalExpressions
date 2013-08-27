#' look for something at the beginning of a line
#' 
#' @export
#' @rdname verEx
startofline <- function(value){
  if (missing(value)){
    outval <- "^"
  } else {
    outval <- paste0("^", value)
  }
  return(verEx(outval))
}

#' look for something at the end of a line
#' @export
#' @rdname verEx
endofline <- function(value){
  if (missing(value)){
    outVal <- "$"
  } else {
    outVal <- paste0(value, "$")
  }
  return(verEx(outVal))
}

#' what to look for
#' @export
#' @rdname verEx
then <- function(value){
  return(verEx(paste0("(?:", value, ")")))
}

#' alias for `then` for start of an expression
#' 
#' @rdname verEx
#' @export
find <- function(value){
  then(value)
}

#' maybe something is there?
#' 
#' @export
#' @rdname verEx
maybe <- function(value){
  return(verEx(paste0("(?:", value, ")?")))
}

#' any character any number of times
#' @export
#' @rdname verEx
anything <- function(value){
  return(verEx("(?:.*)"))
}

#' any character any number of times BUT this
#' @export
#' @rdname verEx
anythingBut <- function(value){
  return(verEx(paste0("(?:[^", value , "]*)")))
}

#' Any character at least one time
#' @export
#' @rdname verEx
something <- function(value){
  return(verEx("(?:.+)"))
}

#' Any character at least one time except for these characters
#' @export
#' @rdname verEx
somethingBut <- function(value){
  return(verEx(paste0("(?:[^", value, "]+)")))
}

#' Line break
#' @export
#' @rdname verEx
linebreak <- function(value){
  return(verEx("(?:(?:\\n)|(?:\\r\\n))"))
}

#' html shorthand
#' @export
#' @rdname verEx
br <- function(value){
  linebreak()
}

#' tab
#' @export
#' @rdname verEx
tab <- function(value){
  return(verEx("\\t"))
}

#' Any alphanumeric
#' @export
#' @rdname verEx
word <- function(value){
  return(verEx("\\w+"))
}

#' Any given character
#' @export
#' @rdname verEx
anyOf <- function(value){
  return(verEx(paste0("[", value, "]")))
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
  return(verEx(paste0("|", tmpVal)))
}

#' verEx
#' 
#' creates an empty vRbalExpression object that can be added to
#' 
#' vRbalExpressions are added together by calling the various constructors. Alternatively, an empty expression can be created by the call to `verEx()`. 
#' 
#' @aliases vRbalExpression
#' @rdname verEx
#'  
#' @examples
#' testExpr <- startofline() + then("http") + maybe("s") + then("://") + maybe("www.") + anythingBut(" ") + endofline()
#' testWWW <- "https://www.google.com"
#' grepl(testExpr, testWWW)
#' 
#' @export
verEx <- function(x){
  if (missing(x)){
    x <- ""
  } 
  class(x) <- "vRbalExpression"
  return(x)
} 


#' Reports whether x is a ggplot object
#' @param x An object to test
#' @export
is.verEx <- function(x) inherits(x, "vRbalExpression")

#' add two vRbalExpression objects together
#' 
#' @export
#' @method + vRbalExpression
"+.vRbalExpression" <- function(v1, v2){
  if (is.verEx(v1) & is.verEx(v2)){
    newV <- paste0(v1, v2)
    verEx(newV)
  } else {
    stop("Can only add two vRbalExpression objects!", .call=FALSE)
  }
}

"%+%" <- `+.vRbalExpression`

#' print vRbalExpression
#' 
#' @export
#' @method print vRbalExpression
print.vRbalExpression <- function(x){
  x <- unclass(x)
  print(x)
  invisible(x)
}