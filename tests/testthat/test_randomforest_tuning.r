context("caret-based model tuning")

test_that("tuning a random forest with caret works", {
  data(rftuning)
  require(randomForest)
  require(caret)
  require(doMC)
  
  
  # set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
  num_cores <- get_parallel_cores_given_os(dev=TRUE)
  registerDoMC(cores = num_cores)
  
  # make this repeatable - comment this out or change it to get a fresh analysis result
  seed_value <- 23581321
  set.seed(seed_value)
  
  # tuning grid of parameters and tuning cross-validation parameters
  fit_grid <- expand.grid(mtry=seq(from=2, to=12, by=4))
  cv_num <- 4
  fit_control <- trainControl(method="cv", 
                              number=cv_num, 
                              #repeats=cv_repeats, 
                              allowParallel = TRUE,
                              ## Estimate class probabilities
                              classProbs = TRUE)
  
  # Set up sampling of train and test data sets
  training_set_fraction <- 0.8
  test_set_fraction <- 1.0 - training_set_fraction
  
  exclude_columns <- c("simulation_run_id", "innovation_rate", "model_class_label", "sample_size", "ta_duration")
  
  model <- train_randomforest(test_train, training_set_fraction, fit_grid, fit_control, exclude_columns)
  
  
  
  
  
  observed <- length(model)
  expected <- 4
  
  expect_equal(expected, observed)
})


test_that("tuning a gradient boosted model with caret works", {
  data(rftuning)
  require(caret)
  require(doMC)
  
  
  # set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
  num_cores <- get_parallel_cores_given_os(dev=TRUE)
  registerDoMC(cores = num_cores)
  
  # make this repeatable - comment this out or change it to get a fresh analysis result
  seed_value <- 23581321
  set.seed(seed_value)
  
  # Set up sampling of train and test data sets
  training_set_fraction <- 0.8
  test_set_fraction <- 1.0 - training_set_fraction
  
  exclude_columns <- c("simulation_run_id", "innovation_rate", "sample_size", "ta_duration")
  
  model <- train_gbm_classifier(test_train, training_set_fraction, "model_class_label", exclude_columns)
  
  observed <- length(model)
  expected <- 4
  
  expect_equal(expected, observed)
})





















