# testing vRbalExpressions

test_that("something works", {
  useExpr <- verbalExpression(something=NULL)
  testString <- ""
  
  expect_false(grepl(useExpr, testString))
  
  testString <- "a"
  expect_true(grepl(useExpr, testString))
})

test_that("anything works", {
  useExpr <- verbalExpression(startofline=NULL, anything=NULL)
  testString <- "what"
  expect_true(grepl(useExpr, testString))
})

test_that("anythingBut works", {
  useExpr <- verbalExpression(startofline=NULL, anythingBut="w")
  testString <- "what"
  expect_true(grepl(useExpr, testString))
})

test_that("somethingBut works", {
  useExpr <- verbalExpression(somethingBut="a")
  testString <- ""
  expect_false(grepl(useExpr, testString))
  testString <- "b"
  expect_true(grepl(useExpr, testString))
  testString <- "a"
  expect_false(grepl(useExpr, testString))
})

test_that("startofline works", {
  useExpr <- verbalExpression(startofline=NULL, then="a")
  testString <- "a"
  expect_true(grepl(useExpr, testString))
  
  testString <- "ba"
  expect_false(grepl(useExpr, testString))
})

test_that("endofline works", {
  useExpr <- verbalExpression(find="a", endofline=NULL)
  testString <- "a"
  expect_true(grepl(useExpr, testString))
  
  testString <- "ab"
  expect_false(grepl(useExpr, testString))
  
  useExpr <- verbalExpression(endofline="a")
  testString <- "a"
  expect_true(grepl(useExpr, testString))
  
  testString <- "ab"
  expect_false(grepl(useExpr, testString))
})

test_that("maybe works", {
  useExpr <- verbalExpression(startofline=NULL, then="a", maybe="b")
  
  testString <- "acb"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc"
  expect_true(grepl(useExpr, testString))
})

test_that("anyOf works", {
  useExpr <- verbalExpression(startofline=NULL, then="a", anyOf="xyz")
  testString <- "ay"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc"
  expect_false(grepl(useExpr, testString))
})

test_that("or works", {
  useExpr <- verbalExpression(startofline=NULL, then="abc", or="def")
  
  testString <- "defzzz"
  expect_true(grepl(useExpr, testString))
  testString <- "abczzz"
  expect_true(grepl(useExpr, testString))
  
  testString <- "xyzabc"
  expect_false(grepl(useExpr, testString))
})

test_that("linebreak works", {
  useExpr <- verbalExpression(startofline=NULL, then="abc", linebreak=NULL, then="def")
  
  testString <- "abc\r\ndef"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc\r\n def"
  expect_false(grepl(useExpr, testString))
})

test_that("br works", {
  useExpr <- verbalExpression(startofline=NULL, then="abc", br=NULL, then="def")
  testString <- "abc\r\ndef"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc\r\n def"
  expect_false(grepl(useExpr, testString))
})

test_that("tab works", {
  useExpr <- verbalExpression(startofline=NULL, tab=NULL, then="abc")
  
  testString <- "\tabc"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc"
  expect_false(grepl(useExpr, testString))
})

