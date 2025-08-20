#' @rdname fractions
#' @aliases approx_rational
#' @title Fractions
#' @description 
#' Finds rational approximations to the components of a real numeric object, using a standard continued fraction method.
#' Calls [MASS::fractions()] (Please refer to that for further details).
#'
#' @param x any object of the numeric mode (missing values are allowed)
#' @param cycles the maximum number of steps to be used in the continued fraction approximation process
#' @param max.denominator an early termination criterion. If any partial denominator exceeds `max.denominator`, the continued fraction stops at that point
#' @param ... further arguments 
#'
#' @return An object of the class `fractions`. A structure with a `.Data` component, the same as the numeric `x` input, 
#' but with the rational approximations held as the character vector attribute `fracs`. Arithmetic operations on `fractions` objects are possible.
#' @export
#' @md
#'
#' @examples
#' X <- matrix(runif(25), 5, 5)
#' fractions(X) #;)
#' fractions(solve(X, X/5))
#' fractions(solve(X, X/5)) + 1
fractions <- function (x, cycles = 10, max.denominator = 2000, ...) {
  MASS::fractions(x, cycles, max.denominator, ...)                      
}
#' @rdname fractions
#' @export approx_rational
# approx_rational <- function(...){
#  fractions(...)}
approx_rational <- fractions
