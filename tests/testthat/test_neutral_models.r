# test neutral_models

context("neutral model functions")

test_that("calculating expected K for a population in a WFIA model works", {
  n <- 2000
  t <- 20
  
  observed <- expected_k_pop(n,t)

  expected <- 81.2528
  
  expect_equal(expected, observed, tolerance=0.1)
})