#' Construct a verbal expression
#' 
#' Take a series of named arguments, and return the actual verbal expression for use with R's various regular expression functions
#' 
#' @export
verbalExpression <- function(...){
  inArgs <- list(...)
  
  nItem <- length(inArgs)
  itemNames <- names(inArgs)
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
then <- function(value){
  return(paste0("(?:", value, ")"))
}

#' alias for then for start of an expression
#' 
find <- function(value){
  then(value)
}

#' maybe something is there?
#' 
#' @export
maybe <- function(value){
  return(paste0("(?:", value, ")?"))
}

#' any character any number of times
#' @export
anything <- function(value){
  return("(?:.*)")
}

#' any character any number of times BUT this
#' @export
anythingBut <- function(value){
  return(paste0("(?:[^", value , "]*)"))
}

#' Any character at least one time
#' @export
something <- function(value){
  return("(?:.+)")
}

#' Any character at least one time except for these characters
#' @export
somethingBut <- function(value){
  return(paste0("(?:[^", value, "]+)"))
}

#' Line break
#' @export
linebreak <- function(){
  return("(?:(?:\\n)|(?:\\r\\n))")
}

#' html shorthand
#' @export
br <- function(){
  linebreak()
}

#' tab
#' @export
tab <- function(){
  return("\\t")
}

#' Any alphanumeric
#' @export
word <- function(){
  return("\\w+")
}

#' Any given character
#' @export
anyOf <- function(value){
  return(paste0("[", value, "]"))
}

#' Any given character (short)
#' @export
any <- function(value){
  anyOf(value)
}

#' Range
#' @export
range <- function(from, to){
  return(paste0("[", from, "-", to, "]"))
}

#' or
#' @export
or <- function(value){
  
}