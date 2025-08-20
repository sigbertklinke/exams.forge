#' @title Histogram Widths
#' @rdname histwidth
#' @aliases width_breaks 
#' @aliases dhistwidth
#' @description  
#' Generates a set of class breaks and absolute frequencies for the range from `from` to `to`.  
#' Class widths are randomly sampled from the vector `widths`. The total number of classes  
#' (`nb`) must be an integer multiple of `min(widths)`; otherwise, the function stops with an error.  
#' If the initial frequencies (`n`) are too small, they can be scaled by an integer factor.  
#' The routine also checks whether the resulting class densities are terminating decimals.
#'
#' @param from numeric: start value of the range.
#' @param to numeric: end value of the range.
#' @param widths numeric: vector of possible class widths to sample from.
#' @param dmax numeric: maximum denominator allowed when checking fractional densities, see [fractions()].
#' @param maxit integer: maximum number of iterations when attempting to find a suitable break pattern.
#'
#' @return A list containing:
#' \item{breaks}{Numeric vector of class boundaries.}
#' \item{n}{Integer vector of absolute frequencies for each class.}
#' \item{decimal}{Logical, `TRUE` if all densities are terminating decimals.}
#' \item{density}{Numeric vector of class densities.}
#' @export
#'
#' @examples
#' l <- histwidth(1.6, 2.1, widths = c(0.05, 0.1, 0.15, 0.2))
#' x <- histx(l$breaks, l$n)
#' histdata(x, l$breaks)
#' # Fallback: use constant min(widths) if no valid break pattern 
#' # is found within max iterations
#' l <- histwidth(1.6, 2.1, widths=0.05, dmax=10)
#' str(l)
histwidth <- function (from, to, widths, dmax = 2000, maxit=1000) {
  rng <- to-from
  nb  <- rng/min(widths)
  if (!equal(nb, round(nb))) stop(sprintf("`to-from` must be a multiple integer of `min(widths)` but is %f*min(width)", nb))
  maxit1 <- maxit
  while (maxit1 > 0) {
    maxit2 <- maxit
    while (maxit2 > 0) {
      breaks <- cumsum(c(from, sample(widths, nb, replace = TRUE)))
      k      <- which(equal(breaks, to, tol=rng/1e6))
      if (length(k)) {
        breaks <- breaks[1:k]
        break
      }
      maxit2 <- maxit2 -1
    }
    if (maxit2==0) breaks <- cumsum(c(from,rep(min(widths), nb))) # fallback if no solution found
    ws    <- diff(breaks)
    probs <- ddiscrete(ws)
    pw    <- probs/ws
    fracs <- fractions(pw, max.denominator=dmax)
    if (unique_max(pw, tol = 1e-06)) break
    maxit1 <- maxit1 - 1
  }
  denom <- strsplit(attr(fracs, "fracs"), '/', fixed=TRUE)
  denom <- as.integer(sapply(denom, function(e) { if (length(e)==1) 1 else e[2]}))
  n <- lcmval(denom) * probs
  #browser()
  i <- 1
  while(i<dmax) {
    if (all(equal(i * n, round(i * n)))) break
    i <- i+1
  }
  ni      <- if (i==dmax) rep(1, length(breaks)-1) else i*n
  density <- ni/diff(breaks)
  list(breaks = breaks, n = ni, decimal = all(is_terminal(density)), density=density)
}  

#' @rdname histwidth
#' @export
# width_breaks <- function(...){
#  histwidth(...)}
width_breaks <- histwidth

#' @rdname histwidth
#' @export
# dhistwidth <- function(...){
#  histwidth(...)}
dhistwidth <- histwidth