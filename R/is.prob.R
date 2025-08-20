#' @rdname is.prob
#' @aliases is_prob_interval
#' @aliases is_prob
#' @aliases in_range
#' @title Interval Checker 
#' @description Checks if `x` is in an opened or closed interval between `min` and `max`. 
#' The default is set as such, that the chosen interval is an interval of \eqn{(0,1)}. 
#' For example, in the case of `x` being a probability.
#'
#' @param x numeric: values to check
#' @param min numeric: minimal value (default: \code{0})
#' @param max numeric: maximal value (default: \code{1})
#' @param open logical: checks if the left and right borders are open or closed (default: \code{TRUE})
#'
#' @md
#' @return A logical vector with the same length as `x`.
#' @export
#'
#' @examples
#' is.prob(runif(1))
is.prob <- function(x, open=TRUE, min=0, max=1)  {
  if (length(open)==1) open <- c(open, open)
  (if (open[1]) (x>min) else (x>=min)) & (if (open[2]) (x<max) else (x<=max))
}

#' @rdname is.prob
#' @export
# is_prob_interval <- function(...){
#  is.prob(...)}
is_prob_interval <- is.prob

#' @rdname is.prob
#' @export
# is_prob <- function(...){
#  is.prob(...)}
is_prob <- is.prob

#' @rdname is.prob
#' @export
# in_range <- function(...){
#  is.prob(...)}
in_range <- is.prob
