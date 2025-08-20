#' Convert a Vector or Matrix to a Horizontal Table
#'
#' @rdname as_table
#' @aliases toTable
#' @title Convert to Table
#'
#' @description
#' Converts a vector or matrix into a formatted horizontal table using \code{xtable}.
#' The output is returned as a character vector, where each element corresponds
#' to a line of the table, suitable for printing or further formatting.
#'
#' @inheritParams xtable::xtable
#' @param ... Additional arguments passed to \code{print.xtable}, such as
#'   \code{type}, \code{file}, or \code{sanitize.text.function}.
#'
#' @return A character vector, each element representing a line of the table.
#'
#' @importFrom xtable xtable print.xtable
#' @export
#'
#' @examples
#' x <- runif(5)
#' tab <- vec2mat(x, colnames = 1:length(x))
#' as_table(tab)
as_table <- function(x, caption = NULL, label = NULL, align = NULL, digits = NULL,
                     display = NULL, auto = FALSE, ...) {
  xt <- xtable(x, caption = caption, label = label, align = align, digits = digits,
               display = display, auto = auto)
  xt <- print(xt, ...)
  strsplit(xt, "\n", fixed=TRUE)[[1]]
}

#' @rdname as_table
#' @export
# toTable <- function(...){
#  as_table(...)}
toTable <- as_table
