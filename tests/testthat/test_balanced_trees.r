
context("tree calculations")

test_that("balanced tree order calculations return correct results", {
  r_vals <- c(3,4,5)
  h_vals <- c(3,4,5)
  expected <- c(40, 341, 3906)
  observed <- orderBalancedTree(r_vals, h_vals)
  
  expect_equal(expected, observed)
})