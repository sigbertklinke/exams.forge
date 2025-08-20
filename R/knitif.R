#' @rdname knitif
#' @aliases knit_select
#' @title Knitting a Text Argument
#' @description Selects a text argument and returns the knitted result.
#'
#' @param n character: text argument to use
#' @param envir environment: in which code chunks are to be evaluated (default: `[knitr::knit_global]`) 
#' @param ...  character: arguments to choose from
#' 
#' @return A character.
#' @importFrom knitr knit_global knit
#' @export
#'
#' @examples
#' knitif(runif(1)<0.5, 'TRUE'="`r pi`", 'FALSE'="$\\pi=`r pi`$")
knitif <- function (n, ..., envir=knit_global()) {
  n    <- as.character(n)
  args <- list(...)
  if (is.null(args[[n]])) stop(sprintf("'%s' not found in arg list", n))
  knit(text=args[[n]], envir=envir)
}

#' @rdname knitif
#' @export
# knit_select <- function(...){
#  knitif(...)}
knit_select <- knitif
