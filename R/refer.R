#' @rdname refer
#' @aliases refer2vector
#' @title Generate Vector Element Names 
#' @description
#' Creates names for elements of a vector.
#'
#' @param x vector: a vector to create the names for 
#' @param fmt character: format string for \code{sprintf} (default: \code{"\%s_{\%.0f}"}
#' @param to character: base name of elements
#' @param index numeric: vector with indices  (default: \code{1:length(x)})
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x <- runif(5)
#' refer(x)                  # LaTeX default
#' refer(x, fmt="%s[%.0f]")  # R default
refer <- function(x, fmt="%s_{%.0f}", to=deparse(substitute(x)), index=1:length(x)) { 
  sprintf(fmt, to, index)
}

#' @rdname refer
#' @export refer2vector
# refer2vector <- function(...){
#  refer(...)}
refer2vector <- refer
