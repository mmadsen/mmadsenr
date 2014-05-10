
#' @title Order of balanced tree
#' 
#' @description 
#' \code{orderBalancedTree} returns the order (number of vertices) of a balanced tree with breadth r and height h.  If the arguments
#' are vectors, the function returns a vector of orders.
#' @param r Branching factor or breadth of the tree, can be scalar or vector
#' @param h Height factor or depth of the tree, can be scalar or vector
#' @keywords graph,network
#' @export
orderBalancedTree <- Vectorize(function(r, h) {
  total <- 0
  hseq <- seq.int(0, h)
  total <- sum(r ^ hseq)
  return(total)
})






