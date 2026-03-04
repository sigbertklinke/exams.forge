#' Safely extract names from an object
#'
#' Returns the names of an object as a character vector. Optionally enforces that
#' names are non-NULL and non-empty when `strict = TRUE`. The function **does not
#' modify the original object**.
#'
#' @param x An R object (vector, list, etc.). Must not be `NULL`.
#' @param strict Logical; if TRUE, errors if `x` is `NULL`, any names are missing, 
#'   or any names are empty strings.
#'
#' @return A character vector of names, same length as `x`.  
#'   - If `x` is an empty list or zero-length object, returns `character(0)`.  
#'   - If `names(x)` is `NULL` and `strict = FALSE`, returns a vector of empty strings of the same length as `x`.
#'
#' @examples
#' x <- c(a = 1, b = 2)
#' getNames(x)
#' 
#' y <- 1:3
#' getNames(y)
#'
#' z <- c(1, 2); names(z) <- c("a", "")
#' # getNames(z, strict = TRUE) # would throw an error: "`x` has empty name(s)"
#'
#' # Empty list returns character(0)
#' getNames(list(), strict = TRUE)
#'
#' # NULL input triggers an error
#' # getNames(NULL, strict = TRUE)
#'
#' @export
getNames <- function(x, strict = FALSE) {
  if (is.null(x)) stop("`x` is `NULL`")
  nx <- names(x)
  if (strict && length(x)) {
    if (length(x) != length(nx)) stop("`x` is missing name(s)")     
    if (!all(nzchar(nx))) stop("`x` has empty name(s)")
  }
  if (is.null(nx)) nx <- rep("", length(x))
  nx
}
