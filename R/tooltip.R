#' @title Tooltip
#' @rdname tooltip
#' @aliases add_tooltip
#' @description Adds a text tooltip to the HTML matrix.
#'
#' @param x an html_matrix object
#' @param tooltip character: text to show (default: \code{NULL})
#'
#' @return An html_matrix object
#'
#' @examples
#' library("magrittr")
#' library("tools")
#' m    <- matrix(1:12, ncol=4)
#' hm   <- html_matrix_sk(m, title='', fmt=rep("%f", ncol(m))) %>% 
#'           add_tooltip(sprintf("Table has %0.f rows and %0.f columns", nrow(.), ncol(.)))
#' if (interactive()) html <- toHTML(hm, browser=TRUE)
tooltip <- function(x, tooltip=NULL) {
  attr(x, "tooltip") <- tooltip
  x
}

#' @rdname tooltip
#' @export
# add_tooltip <- function(...){
#  tooltip(...)}
add_tooltip <- tooltip
