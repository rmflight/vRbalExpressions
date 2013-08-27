\name{startofline}
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
\alias{vRbalExpression}
\alias{verEx}
\alias{word}
\title{look for something at the beginning of a line}
\usage{
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

  verEx(x)
}
\description{
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

  creates an empty vRbalExpression object that can be added
  to
}
\details{
  vRbalExpressions are added together by calling the
  various constructors. Alternatively, an empty expression
  can be created by the call to `verEx()`.
}
\examples{
testExpr <- startofline() + then("http") + maybe("s") + then("://") + maybe("www.") + anythingBut(" ") + endofline()
testWWW <- "https://www.google.com"
grepl(testExpr, testWWW)
}

