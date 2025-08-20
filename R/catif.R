#' @title Conditional Cat Output
#' @description Prints text using \code{cat} only when a specified logical condition is \code{TRUE}.
#' 
#' @param cond Logical value. If \code{TRUE}, the text provided in \code{...} is printed using \code{cat}; if \code{FALSE}, nothing is printed.
#' @param ... Additional arguments passed to \code{cat}.
#' @return Invisibly returns the value of \code{cond}.
#' @rdname catif
#' @aliases condition_cat
#' @export
#' 
#' @examples
#' catif(TRUE, "PDF")          # This text is printed
#' catif(FALSE, "Moodle")      # Nothing is printed
#' condition_cat(TRUE, "Hello") # Alias works the same way
catif <- function(cond, ...) {
  if (as.logical(cond)) cat(...)
  invisible(cond)
}

#' @rdname catif
#' @export
# condition_cat <- function(...){
#  catif(...)}
condition_cat <- catif
