#' @rdname inline
#' @aliases txt_knit
#' @title Text Knitting 
#' @description Knits `txt` within an R code chunk.
#' 
#' @param txt character
#'
#' @return Output.
#' @importFrom knitr knit knit_global
#' @export
#'
#' @examples
#' result <- inline("2 + 2")
inline <- function(txt) {
  cat(knitr::knit(text=txt, envir = knitr::knit_global(), quiet=TRUE))
}

#' @rdname inline
#' @export
# txt_knit <- function(...){
#  inline(...)}
txt_knit <- inline
