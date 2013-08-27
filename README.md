# vRbalExpressions

Port of the [verbal expressions](http://verbalexpressions.github.io/) library in [R](www.r-project.org).

## Notes

  * No sanitizing of inputs!
  * Returns a `character` vector that can be used as input to the `regex` functions

## Planned Improvements

  * More tests!
    * Functions without tests: `range`, `word`
  * Sanitize inputs
  
## Installation

```
library(devtools)
install_github("vRbalExpressions", "rmflight")
library(vRbalExpressions)
```

## Examples

```
library(vRbalExpressions)

testExpr <- startofline() + then("http") + maybe("s") + then("://") + maybe("www.") + anythingBut(" ") + endofline()

testWWW <- "https://www.google.com"

grepl(testExpr, testWWW)
```
## License

CC0
