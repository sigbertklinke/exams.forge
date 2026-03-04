#' @rdname num_result
#' @aliases num_res
#' @title Numeric Rounding List
#' @description 
#' `num_result` creates a list summarizing numeric values with rounding and tolerance. 
#' It returns the original values, rounded values, the digits used, and the tolerance. 
#' 
#' The rounding is done with the internal function `fmt()` (similar to `exams::fmt()`). 
#' Users can pass additional arguments to `fmt()` via `...`.
#' 
#' * `x`: original numeric values
#' * `fx`: rounded values as character (via `fmt()`)
#' * `tolerance`: numeric tolerance for comparison
#' * `digits`: digits used for rounding
#' 
#' If `digits` is not provided:
#' * If `length(x) > 1`, `ceiling(-log10(min(diff(sort(x)), na.rm = TRUE)))` is used.
#' * If `length(x) == 1`, `3 + ceiling(-log10(abs(x)))` is used.
#' 
#' If `tolerance` is not provided, it defaults to `tolmult * 10^(1 - digits)`.
#' 
#' `int_result()` is a shortcut for integer values (`digits = 0`, `tolerance = 0.1`).
#' 
#' @param x numeric: the input values
#' @param digits numeric: number of digits to round to (default: `NULL`)
#' @param tolerance numeric: optional numeric tolerance (default: `NULL`)
#' @param tolmult numeric: multiplier for tolerance calculation (default: 2)
#' @param ... further arguments passed to `fmt()`. Common arguments include:
#'   * `digits`: number of digits for formatting
#'   * `zeros`: logical; pad with trailing zeros (default TRUE for digits < 4)
#' 
#' @return A list with elements `x`, `fx`, `tolerance`, and `digits`.
#' @md
#' @export
#'
#' @examples
#' # Example: numeric values
#' x <- rnorm(10, mean = 1.8, sd = 0.25)
#' num_result(c(mean(x), x), digits = 2)
#'
#' # Example: integer result
#' int_result(mean(x))
#'
#' # Example: different digits and tolerance
#' num_result(pi, 3)
#' num_result(pi, 6)
#' num_result(pi, 6, tolmult = 5)
#' num_result(pi, 6, tolmult = 5, tolerance = 1e-6)
num_result <- function (x, digits = NULL, tolerance = NULL, tolmult = 2, ...) { 
  if (is.null(digits)) {
    if (length(x) < 2) {
      if ((x < 0) || (x > 0)) 
        digits <- 3 + ceiling(-log10(abs(x)))
      if (is.null(digits)) digits <- 4
    } else {
      digits <- ceiling(-log10(min(diff(sort(x)), na.rm = TRUE)))
    }
  }
  tolerance <- if (is.null(tolerance)) 
    tolmult * 10^(1 - digits)
  else tolmult * tolerance
  list(x = x, fx = fmt(x, digits = digits, ...), tolerance = tolerance,  digits = digits)
}

#' @rdname num_result
#' @aliases int_res
#' @export
int_result <- function(x, ...) { num_result(as.integer(x), digits=0, tolerance=0.1, tolmult=1, ...) }


#' @rdname num_result
#' @export
# num_res <- function(...){
#  num_result(...)}
num_res <- num_result

#' @rdname num_result
#' @export
# int_res <- function(...){
#  int_result(...)}
int_res <- int_result
