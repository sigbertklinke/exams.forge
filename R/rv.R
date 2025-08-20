#' @rdname rv
#' @aliases rmdFormatRV, lrv
#' @title Random Variable
#' @description
#' Formats a random variable and its meaning for R Markdown.
#'
#' @param symbol character: symbol
#' @param explanation character: meaning
#'
#' @return A formatted string.
#' @export
#'
#' @examples
#' rv("X", "Waiting time in minutes until next event")
rv <- function(symbol, explanation) {
  sprintf('$%s$: "%s"', symbol, explanation)
} 

#' @rdname rv
#' @export rmdFormatRV
# rmdFormatRV <- function(...){
#  rv(...)}
rmdFormatRV <- rv

#' @rdname rv
#' @export 
# lrv <- function(...){
#  rv(...)}
lrv <- rv
