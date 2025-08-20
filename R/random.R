#' @rdname random
#' @aliases rand
#' @title Random
#' @description Returns a index from \code{1:length(v)} randomly ordered.
#'
#' @param v vector: vector with elements
#'
#' @return Index
#' @export
#'
#' @examples
#' random(-3:3)
random <- function(v) {
  sample(length(v), size=length(v)) 
}

#' @rdname random
#' @export
# rand <- function(...){
#  random(...)}
rand <- random
