#' @title Sanitization
#' @rdname nosanitize
#'
#' @description
#' \code{nosanitize} makes no sanitization on the strings.
#'
#' @param str character: vector to sanitize
#'
#' @return A sanitized character vector.
#' @export
#'
#' @examples
#' nosanitize("Test")
nosanitize <- function(str) { str }
