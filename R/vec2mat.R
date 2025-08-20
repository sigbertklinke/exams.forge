#' @rdname vec2mat
#' @title Vector to Matrix Conversion
#' @aliases to_mat
#' 
#' @description Converts a vector to a horizontal or vertical matrix and sets `row-` or `colnames`. 
#' If \code{rownames} or \code{colnames} are given, then existing row names or column names are overwritten.
#'
#' @param x vector
#' @param colnames character: vector of new column names (default: \code{NULL})
#' @param rownames character: vector of new row names (default: \code{NULL})
#' @param horizontal logical: horizontal or vertical matrix (default: \code{TRUE})
#'
#' @return A matrix
#' @export
#'
#' @examples
#' x <- runif(5)
#' vec2mat(x)
#' vec2mat(x, horizontal=FALSE)
vec2mat <- function(x, colnames=NULL, rownames=NULL, horizontal=TRUE) {
  x <- matrix(x, nrow=ifelse(horizontal, 1, length(x)), ncol=ifelse(horizontal, length(x), 1))
  if (!is.null(colnames)) colnames(x) <- colnames
  if (!is.null(rownames)) rownames(x) <- rownames
  x
}

#' @rdname vec2mat
#' @export
# to_mat <- function(...){
#  vec2mat(...)}
to_mat <- vec2mat
