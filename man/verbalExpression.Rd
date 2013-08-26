\name{verbalExpression}
\alias{verbalExpression}
\title{Construct a verbal expression}
\usage{
  verbalExpression(...)
}
\value{
  character string with the constructed regular expression
}
\description{
  Take a series of named arguments, and return the actual
  verbal expression for use with R's various regular
  expression functions
}
\examples{
testExpr <- verbalExpression(startofline=NULL, then="http",maybe="s",then="://",maybe="www.",anythingBut=" ",endofline=NULL)
testWWW <- "https://www.google.com"
grepl(testExpr, testWWW)

testExpr <- verbalExpression(find="bird")
sub(testExpr, "duck", "Replace bird with duck")
}

