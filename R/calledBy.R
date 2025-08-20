#' @title Check if a Function Was Called by Another Function
#' @description
#' Determines whether the current function call was initiated by a specified function.
#' This is useful for conditional behavior depending on the caller.
#'
#' @param fun Character string specifying the name of the calling function to check for.
#'   Defaults to `"exams2pdf"`.
#'
#' @return A logical value: `TRUE` if the current call was triggered by `fun`, otherwise `FALSE`.
#'
#' @rdname calledBy
#' @aliases called_by
#' @export
#'
#' @examples
#' funB <- function() { calledBy("funA") }
#' funA <- function() { funB() }
#' funA()  # Returns TRUE because funB was called by funA
calledBy <- function(fun='exams2pdf') {
  any(sapply(sys.calls(), function(e) { as.character(e)[1] })==fun)
}

#' @rdname calledBy
#' @export
# called_by <- function(...){
#  calledBy(...)}
called_by <- calledBy
