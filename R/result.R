#' Results with Rounding
#'
#' Creates a structured result with a numeric value rounded according to specified digits,
#' an optional tolerance, and a rounding function.
#' 
#' By default, rounding is performed using an **internal function `round2()`**, which is similar
#' to \code{exams::round2()}, but users should not call it directly. You can also supply a
#' custom rounding function via the `FUN` argument.
#'
#' @rdname as_result
#' @aliases as_res
#' @aliases tolerance
#' @title Results with Rounding
#' 
#' @description
#' Rounds \code{x} according to \code{digits} and the rounding function \code{FUN}, and sets
#' a tolerance for the result. If \code{tol} is not provided, it defaults to \code{2*10^(-digits)}.
#' 
#' @param x numeric: value to round
#' @param digits integer or character: Number of digits to use for rounding, see Details.
#' @param tol numeric: tolerance for the result (defaults to \code{2*10^(-digits)} if NA)
#' @param FUN function: rounding function (default: internal \code{round2()})
#' @details
#' If \code{digits} is a character, the following abbreviations are recognized:
#' \describe{
#'   \item{\code{"integer"}}{digits = 0}
#'   \item{\code{"\%"}}{digits = 2}
#'   \item{\code{"probability"}}{digits = 4}
#' }
#' Partial matching of these names is allowed.
#' @return A list of class \code{result} containing:
#' \item{x}{Original value}
#' \item{r}{Rounded value}
#' \item{digits}{Digits used for rounding}
#' \item{tol}{Tolerance for the result}
#' 
#' @export
#'
#' @examples
#' x <- as_result(1/3, "probability")
#' tol(x)
#' rounded(x)
#' digits(x)
as_result <- function(x, digits, tol=NA, FUN=round2) {
  stopifnot(is.numeric(x))
  if (is.character(digits)) {
    pos <- pmatch(digits, c("integer", "probability", "%"))
    if (pos==1) digits <- 0
    if (pos==2) digits <- 4
    if (pos==3) digits <- 2
    stopifnot(is.numeric(digits))
  }
  tol[is.na(tol)] <- 2*10^(-digits)
  ret <- list(x=x, r=FUN(x, digits), digits=digits, tol=max(tol))
  structure(ret, class=c("result", class(ret)))
}

#' @rdname as_result
#' @export
tol <- function(x) {
  stopifnot("result" %in% class(x))
  x$tol
}

#' @rdname as_result
#' @export
rounded <- function(x) {
  stopifnot("result" %in% class(x))
  x$r
}

#' @rdname as_result
#' @export
val <- function(x) {
  stopifnot("result" %in% class(x))
  x$x
}

#' @rdname as_result
#' @export
digits <- function(x) {
  stopifnot("result" %in% class(x))
  x$digits
}

#' @rdname as_result
#' @export
# as_res <- function(...){
#  as_result(...)}
as_res <- as_result

#' @rdname as_result
#' @export
# tolerance <- function(...){
#  tol(...)}
tolerance <- tol
