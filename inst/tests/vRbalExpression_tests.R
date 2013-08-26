# testing vRbalExpressions

test_that("something works", {
  useExpr <- verbalExpression(something=NULL)
  testString <- ""
  
  expect_that(-1, is_equivalent_to(regexpr(useExpr, testString)))
  
  testString <- "a"
  expect_that(1, is_equivalent_to(regexpr(useExpr, testString)))
})

test_that("anything works", {
  useExpr <- verbalExpression(startofline=NULL, anything=NULL)
  testString <- "what"
  expect_that(1, is_equivalent_to(regexpr(useExpr, testString)))
})

test_that("anythingBut works", {
  useExpr <- verbalExpression(startofline=NULL, anythingBut="w")
  testString <- "what"
  expect_that(1, is_equivalent_to(regexpr(useExpr, testString)))
})

test_that("somethingBut works", {
  useExpr <- verbalExpression(somethingBut="a")
  testString <- ""
  expect_that(-1, is_equivalent_to(regexpr(useExpr, testString)))
  testString <- "b"
  expect_that(1, is_equivalent_to(regexpr(useExpr, testString)))
  testString <- "a"
  expect_that(-1, is_equivalent_to(regexpr(useExpr, testString)))
})

test_that("startofline works", {
  useExpr <- verbalExpression(startofline=NULL, then="a")
  testString <- "a"
  expect_equivalent(1, regexpr(useExpr, testString))
  
  testString <- "ba"
  expect_equivalent(-1, regexpr(useExpr, testString))
})

test_that("endofline works", {
  useExpr <- verbalExpression(find="a", endofline=NULL)
  testString <- "a"
  expect_equivalent(1, regexpr(useExpr, testString))
  
  testString <- "ab"
  expect_equivalent(-1, regexpr(useExpr, testString))
  
  useExpr <- verbalExpression(endofline="a")
  testString <- "a"
  expect_equivalent(1, regexpr(useExpr, testString))
  
  testString <- "ab"
  expect_equivalent(-1, regexpr(useExpr, testString))
})

test_that("maybe works", {
  useExpr <- verbalExpression(startofline=NULL, then="a", maybe="b")
  
  testString <- "acb"
  expect_equivalent(1, regexpr(useExpr, testString))
  
  testString <- "abc"
  expect_equivalent(1, regexpr(useExpr, testString))
})

test_that("anyOf works", {
  useExpr <- verbalExpression(startofline=NULL, then="a", anyOf="xyz")
  testString <- "ay"
  expect_equivalent(1, regexpr(useExpr, testString))
  
  testString <- "abc"
  expect_equivalent(-1, regexpr(useExpr, testString))
})

test_that("or works", {
  
})