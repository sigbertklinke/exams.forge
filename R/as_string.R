#' @rdname as_string
#' @aliases lstring lobs as_obs as_sum as_fraction lfrac
#' @title Convert Vectors to Strings or Formatted Representations
#' @description 
#' These functions convert vectors into human-readable string representations.
#' They can join elements, create LaTeX-formatted fractions, or label observations.
#'
#' @param txt Character vector to merge into a single string (used in \code{as_string}, \code{as_obs}).
#' @param val Numeric vector of values to convert into fractions (used in \code{as_fraction}).
#' @param collapse Character string inserted between elements (default: \code{", "}).
#' @param last Character string used between the last two elements (default: \code{", and "}).
#' @param name Character string used as the observation name (default: \code{"x"}; used in \code{as_obs}).
#' @param latex Logical; if \code{TRUE}, returns fractions in LaTeX format \code{\\frac{.}{.}} (default: \code{FALSE}; used in \code{as_fraction}).
#' @param sorted Logical; if \code{TRUE}, sort the vector before conversion (default: \code{FALSE}).
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A single string, or a vector of formatted strings (for fractions in \code{as_fraction}).
#'
#' @export
#'
#' @examples
#' x <- runif(5)
#' y <- c(TRUE, FALSE, NA)
#'
#' # Basic string conversion
#' as_string(x)
#' as_string(y)
#' as_string(as.character(x))
#' as_string(as.character(y))
#'
#' # Observations
#' as_obs(x)
#' as_obs(sort(x), sorted = TRUE)
#'
#' # Fraction conversion
#' x <- round(runif(5), 2)
#' as_fraction(x)
#' as_fraction(x, latex = TRUE)
#'
#' # Summing elements as a string
#' y <- round(runif(5), 2)
#' as_sum(y)
as_string <- function (txt, collapse=", ", last=", and ") {
  n <- length(txt)-1
  collapse <- c(rep(collapse, length.out=n), '')
  if (is.character(last)) collapse[n] <- last
  paste0(paste0(txt, collapse), collapse="")
}

#' @rdname as_string
#' @export
as_sum <- function (txt) { as_string(txt, collapse="+", last="+") }

#' @rdname as_string
#' @export
as_obs <- function(txt, name="x", sorted=FALSE, ...) {
  txt <- paste0('$', name, '_{', if(sorted) '(' else '', 1:length(txt), if(sorted) ')' else '', '}=', txt, '$')
  as_string(txt, ...)
}

#' @rdname as_string
#' @export
as_fraction <- function(val, latex=FALSE, sorted=FALSE, ...) {
  pfrac <- function(v) {
    if (length(v)==1) return(v)
    paste0("\\frac{", v[1], "}{", v[2], "}")    
  }
  #
  if (sorted) val <- sort(val)
  f  <- fractions(val)
  ff <- attr(f, "fracs")  
  if (latex) ff <- sapply(strsplit(ff, "/"), pfrac)
  ff
}

#' @rdname as_string
#' @export
# lobs <- function(...){
#  as_obs(...)}
lobs <- as_obs

#' @rdname as_string
#' @export
# lstring <- function(...){
#  as_string(...)}
lstring <- as_string

#' @rdname as_string
#' @export
# lfrac <- function(...){
#  as_fraction(...)}
lfrac <- as_fraction
