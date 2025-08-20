#' @rdname equal
#' @aliases approx_equal
#' @title Conditional Value Matching
#' @description It performs a comparison by checking if either `abs(x - y) < tol` when `outer == FALSE`, 
#' or if an `a` exists or a  `y[j]` for each `x[i]` such that the condition `abs(x[i] - y[j]) < tol` is satisfied.
#' @param x numeric
#' @param y numeric
#' @param tol numeric: tolerance (default: \code{1e-6})
#' @param outer logical: compares directly or verifies whether `x` is present within `y` (default: FALSE).
#'
#' @return logical
#' @export
#'
#' @examples
#' equal(9*1/9, 1)
equal <- function(x, y, tol=1e-6, outer=FALSE) {
#  abs(x-y)<tol
  if (is.na(tol) || (tol<0)) tol <- 1e-6
  if (outer) {
    ret <- apply(abs(outer(x, y, '-'))<tol, 1, any)  
  } else {
    ret <- abs(x - y) < tol
  }
  ret
}

#' @rdname equal
#' @export
# approx_equal <- function(...){
#  equal(...)}
approx_equal <- equal
