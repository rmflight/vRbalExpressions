# testing vRbalExpressions

test_that("something works", {
  useExpr <- something()
  testString <- ""
  
  expect_false(grepl(useExpr, testString))
  
  testString <- "a"
  expect_true(grepl(useExpr, testString))
})

test_that("anything works", {
  useExpr <- startofline() + anything()
  testString <- "what"
  expect_true(grepl(useExpr, testString))
})

test_that("anythingBut works", {
  useExpr <- startofline() + anythingBut("w")
  testString <- "what"
  expect_true(grepl(useExpr, testString))
})

test_that("somethingBut works", {
  useExpr <- somethingBut("a")
  testString <- ""
  expect_false(grepl(useExpr, testString))
  testString <- "b"
  expect_true(grepl(useExpr, testString))
  testString <- "a"
  expect_false(grepl(useExpr, testString))
})

test_that("startofline works", {
  useExpr <- startofline() + then("a")
  testString <- "a"
  expect_true(grepl(useExpr, testString))
  
  testString <- "ba"
  expect_false(grepl(useExpr, testString))
})

test_that("endofline works", {
  useExpr <- find("a") + endofline()
  testString <- "a"
  expect_true(grepl(useExpr, testString))
  
  testString <- "ab"
  expect_false(grepl(useExpr, testString))
  
  useExpr <- endofline("a")
  testString <- "a"
  expect_true(grepl(useExpr, testString))
  
  testString <- "ab"
  expect_false(grepl(useExpr, testString))
})

test_that("maybe works", {
  useExpr <- startofline() + then("a") + maybe("b")
  
  testString <- "acb"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc"
  expect_true(grepl(useExpr, testString))
})

test_that("anyOf works", {
  useExpr <- startofline() + then("a") + anyOf("xyz")
  testString <- "ay"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc"
  expect_false(grepl(useExpr, testString))
})

test_that("or works", {
  useExpr <- startofline() + then("abc") + or("def")
  
  testString <- "defzzz"
  expect_true(grepl(useExpr, testString))
  testString <- "abczzz"
  expect_true(grepl(useExpr, testString))
  
  testString <- "xyzabc"
  expect_false(grepl(useExpr, testString))
})

test_that("linebreak works", {
  useExpr <- startofline() + then("abc") + linebreak() + then("def")
  
  testString <- "abc\r\ndef"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc\r\n def"
  expect_false(grepl(useExpr, testString))
})

test_that("br works", {
  useExpr <- startofline() + then("abc") + br() + then("def")
  testString <- "abc\r\ndef"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc\r\n def"
  expect_false(grepl(useExpr, testString))
})

test_that("tab works", {
  useExpr <- startofline() + tab() + then("abc")
  
  testString <- "\tabc"
  expect_true(grepl(useExpr, testString))
  
  testString <- "abc"
  expect_false(grepl(useExpr, testString))
})

test_that("verEx adding works", {
  useExpr <- startofline() + then("http") + maybe("s") + then("://") + maybe("www.") + anythingBut(" ") + endofline()
  testWWW <- "https://www.google.com"
  expect_true(grepl(useExpr, testWWW))
})

