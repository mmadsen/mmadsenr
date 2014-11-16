context("model training and tuning")

test_that("confusion matrix parsing works", {
  data(confusionmatrix)
  
  res <- get_parsed_binary_confusion_matrix_stats(cm)
  
  observed <- res$kappa[1]
  expected <- 0.2946
  
  expect_equal(expected, observed, tolerance=0.01)
})