
#' @title get_parallel_cores_given_os
#' @description
#' Returns the number of cores to be used for parallel/doMC processing, given the 
#' operating system.  On OSX ("Darwin"), this returns half the number of cores 
#' under the assumption that this is a development environment.  This behavior 
#' can be turned off with the argument dev = FALSE.  
#' 
#' @param boolean development environment?  defaults to TRUE, and applies to Darwin
#' @return number number of cores to use for parallel processing.  Defaults to all available.  
#' @export

get_parallel_cores_given_os <- function(dev = TRUE) {
  require("parallel")
  os <- Sys.info()['sysname']
  cores <- detectCores()
  if( os == 'Darwin' && dev == TRUE) {
    cores <- cores / 2
  }
  cores
}