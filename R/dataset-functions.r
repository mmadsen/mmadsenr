# utility functions for data sets

#' Randomly splits a data frame into a training and test set, with a given fraction of rows in the 
#' test data set.    
#' 
#' @param df data frame to split
#' @param test_fraction 
#' @return list List with "train" and "test" elements
#' @export

random_split_dataset <- function(df, test_fraction) {
  shuffled_df <- df[sample(nrow(df)),]
  indexes = sample(1:nrow(shuffled_df), size=test_fraction*nrow(shuffled_df))
  test = shuffled_df[indexes,]
  train = shuffled_df[-indexes,]
  ret <- list("train" = train, "test" = test)
  ret
}

