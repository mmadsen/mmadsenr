
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


#' @title list_files_for_data_path
#' @description 
#' Lists files for a directory, perhaps given a full base directory specified in the 
#' user's environment or in an environment variable.  The goal is to allow the same
#' code to be easily run in multiple desktop environments, by multiple developers
#' who may have different disk directory layouts, and then move that code to servers 
#' and compute clusters, simply by changing one variable which points at the "base" 
#' directory in which files might be found.  
#' 
#' If the "basedir" argument is given, either through an absolute path, or more 
#' likely by passing the command line arguments from a script or an environment/startup variable, 
#' then this basedir is prepended to the "directory" variable.  This full absolute path
#' is then used as the starting point to construct a list of files whose names match the 
#' specified pattern.  This list is constructed using the normal base R function list.files(), 
#' and its return value is passed back to the user, so this function is a drop-in replacement.  
#' 
#' @param character Absolute path to a base directory for searching for directories and files
#' @param character Relative directory or directory path within which files should be searched
#' @param character Pattern for searching for files.  
#' @return A character vector containing file names in the specified directories that match the pattern, or "" if none matched.
#' @export


list_files_for_data_path <- function(basedir = "", directory = "", pattern = "") {
  if(basedir != "") {
    p <- c(basedir)
  } else if(Sys.getenv("R_DATA_BASEDIR") != "") {
    p <- c(Sys.getenv("R_DATA_BASEDIR"))
  } else if(exists("data_directory") == TRUE) {
    p <- c(data_directory)
  } else {
    p <- c(getwd())
  } 
  final <- paste(p, directory, sep="/", collapse="")
  list.files(path = final, pattern=pattern, full.names=TRUE)
}



