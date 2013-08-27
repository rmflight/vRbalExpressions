\name{verbalExpression}
\alias{any}
\alias{anyOf}
\alias{anything}
\alias{anythingBut}
\alias{br}
\alias{endofline}
\alias{find}
\alias{linebreak}
\alias{maybe}
\alias{or}
\alias{range}
\alias{something}
\alias{somethingBut}
\alias{startofline}
\alias{tab}
\alias{then}
\alias{verbalExpression}
\alias{word}
\title{Construct a verbal expression}
\usage{
  verbalExpression(...)

  startofline(value)

  endofline(value)

  then(value)

  find(value)

  maybe(value)

  anything(value)

  anythingBut(value)

  something(value)

  somethingBut(value)

  linebreak(value)

  br(value)

  tab(value)

  word(value)

  anyOf(value)

  any(value)

  range(from, to)

  or(value)
}
\value{
  character string with the constructed regular expression
}
\description{
  Take a series of named arguments, and return the actual
  verbal expression for use with R's various regular
  expression functions

  look for something at the beginning of a line

  look for something at the end of a line

  what to look for

  alias for `then` for start of an expression

  maybe something is there?

  any character any number of times

  any character any number of times BUT this

  Any character at least one time

  Any character at least one time except for these
  characters

  Line break

  html shorthand

  tab

  Any alphanumeric

  Any given character

  Any given character (short)

  Range

  or
}
\examples{
testExpr <- verbalExpression(startofline=NULL, then="http",maybe="s",then="://",maybe="www.",anythingBut=" ",endofline=NULL)
testWWW <- "https://www.google.com"
grepl(testExpr, testWWW)

testExpr <- verbalExpression(find="bird")
sub(testExpr, "duck", "Replace bird with duck")
}

