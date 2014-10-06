# functions for neutral models

expected_k_pop <- function(n, theta) {
  
  expected_k_pop_integrand <- function(x) {(1/x) * ((1 - x)^(theta - 1))}
  
  
  
  tmp <- integrate(expected_k_pop_integrand, lower=1/n, upper=1)$value
  e_k <- tmp * theta
  e_k
}






