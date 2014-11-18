
#' @title random_split_dataset
#' @description 
#' Randomly splits a data frame into a training and test set, with a given fraction of rows in the 
#' test data set.    
#' 
#' @param df data frame to split
#' @param test_fraction 
#' @return list List with "train" and "test" elements
#' @export

random_split_dataset <- function(df, test_fraction) {
  require("caret")
  
  shuffled_df <- df[sample(nrow(df)),]
  indexes = sample(1:nrow(shuffled_df), size=test_fraction*nrow(shuffled_df))
  test = shuffled_df[indexes,]
  train = shuffled_df[-indexes,]
  ret <- list("train" = train, "test" = test)
  ret
}

#' @title get_data_path
#' @description
#' Utility function to help analysis be portable across dev environments and EC2 or
#' StarCluster execution environments.  If passed a "basedir", it uses that as the 
#' basis of a fully qualified path.  If not, it infers a basedir by first checking
#' for environment variable "R_DATA_BASEDIR".  If this does not exist, it then checks
#' for the existence of a variable "data_basedir" in the environment (perhaps from an .rprofile).
#' If this doesn't exist, it uses getwd() as the basedir.  It then appends the "suffix" if 
#' provided, and returns the canonical form of the directory path for the operating system.  
#' 
#' @param string basedir (defaults to NULL)
#' @param string suffix (defaults to NULL)
#' @param string filename
#' @param args  List of command line arguments (to be processed by commandArgs())
#' @examples
#' get_data_path(suffix = "experiment", filename = "foo.r") returns {basedir}experiment/foo.r where {basedir} is the current working directory or the value of the "data_directory" variable.
#' get_data_path(basedir = "/home/foo", suffix = "experiment", filename = "foo.r") returns /home/foo/experiment/foo.r
#' get_data_path(filename = "foo.r", args = commandArgs(trailingOnly = TRUE)) takes the basedir from the command line as the first argument
#' 
#' @export

get_data_path <- function(basedir = "", suffix = "", filename = "", args = "") {
  
  if(basedir != "") {
    p <- c(basedir)
  } else if(args != "") {
    p <- args[1]  
  } else if(Sys.getenv("R_DATA_BASEDIR") != "") {
    p <- c(Sys.getenv("R_DATA_BASEDIR"))
  } else if(exists("data_directory") == TRUE) {
    p <- c(data_directory)
  } else {
    p <- c(getwd())
  }
  

  final <- paste(p, suffix, filename, sep="/", collapse="")
  final
}

