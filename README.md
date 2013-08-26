# vRbalExpressions

Port of the [verbal expressions](http://verbalexpressions.github.io/) library in [R](www.r-project.org).

## Notes

  * No sanitizing of inputs!
  * Returns a `character` vector that can be used as input to the `regex` functions

## Planned Improvements

  * Change style to be more like [ggplot2](http://docs.ggplot2.org/current/gg-add.html)
    * This would potentially allow recursion of functions
    * Remove need to pass `NULL` as an argument
    * Allow iterative construction
  * More tests!
    * Functions without tests: `range`, `word`
  * Sanitize inputs

## License

CC0
