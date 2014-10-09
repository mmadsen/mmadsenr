
#' @title expected_k_pop
#' @description
#' Calculates the expected number of alleles at a locus in a population, evolving under
#' a Wright-Fisher infinite-alleles drift model.  Employs the approximation from 
#' Ewens 1972, Equation 4, and performs a simple numerical integration.  The approximation
#' is good enough to match the value from Mathematica 9/10.  
#' 
#' @param n population size
#' @param theta scaled innovation rate 
#' @return expected_k
#' @export

expected_k_pop <- function(n, theta) {
  
  expected_k_pop_integrand <- function(x) {(1/x) * ((1 - x)^(theta - 1))}
  
  
  
  tmp <- integrate(expected_k_pop_integrand, lower=1/n, upper=1)$value
  e_k <- tmp * theta
  e_k
}






