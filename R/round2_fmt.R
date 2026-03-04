#' @title Rounding and Formatting Numbers
#' @description
#' These functions provide rounding and formatting similar to \code{exams::round2} and \code{exams::fmt}.
#' \code{round2} performs half-up rounding with a small offset to avoid rounding errors.
#' \code{fmt} formats numbers as character strings with a specified number of decimal places.
#'
#' @param x Numeric vector to be rounded or formatted.
#' @param digits Integer, number of decimal places. Default is 0 for \code{round2}, 2 for \code{fmt}.
#' @param zeros Logical, whether to keep trailing zeros. Default is TRUE if digits < 4 (\code{fmt} only).
#' @param ... Additional arguments passed to \code{format()} (\code{fmt} only).
#'
#' @return \code{round2} returns a numeric vector rounded to the specified number of digits.  
#' \code{fmt} returns a character vector with numbers formatted as strings with the requested number of decimal places.
#'
#' @seealso \code{\link[exams]{round2}}, \code{\link[exams]{fmt}} from the \code{exams} package.
#' @author Adapted from \code{exams} package by R. Schwabe et al.
#'
#' @name round2_fmt
NULL

#' @rdname round2_fmt
#' @keywords internal 
round2 <- function(x, digits = 0) {
  round(x + sign(x) * 1e-10, digits)
}

#' @rdname round2_fmt
#' @keywords internal
fmt <- function(x, digits = 2L, zeros = digits < 4L, ...) {
  x <- round2(x, digits = digits)
  if (zeros) {
    format(x, nsmall = digits, scientific = FALSE, digits = 12L, ...)
  } else {
    format(x, scientific = FALSE, digits = 12L, ...)
  }
}
