context("data set functions")

test_that("random splitting a data set works", {
  data(iris)
  
  total <- nrow(iris)
  test_fraction <- 0.10
  
  ret <- random_split_dataset(iris, test_fraction)
  
  test <- ret["test"]$test
  
  observed <- nrow(test)
  expected <- total * test_fraction
  
  expect_equal(expected, observed)
})