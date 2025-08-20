#' @title Histogram Widths
#' @rdname histwidth
#' @aliases width_breaks 
#' @aliases dhistwidth
#' @description  Creates a set of breaks and absolute frequencies in the range from '`from`' to '`to`'. The class widths are 
#' sampled from `widths`. The resulting numbers could be multiplied with an integer, if the `sum(n)` is too small.
#' Additionally, it is checked whether the generated densities are terminating decimals.
#'
#' @param from numeric: start value
#' @param to numeric: end value
#' @param widths numeric: a vector of width to sample from
#' @param dmax numeric: max. denominator value
#'
#' @return A list with `breaks`, `n`'s for each class and `decimal` if all densities are terminating decimals.
#' @export
#'
#' @examples
#' l <- histwidth(1.6, 2.1, widths=c(0.05, 0.1, 0.15, 0.2))
#' x <- histx(l$breaks, l$n)
#' histdata(x, l$breaks)
#' 
histwidth <- function(from, to, widths,  dmax=100) {
  is_ter <- function(x) {
    #browser()
    while(any(x>1)) {
      sx <- sum(x)
      x  <- ifelse(x%%2==0, x/2, x)
      x  <- ifelse(x%%5==0, x/5, x)
      if (sum(x)==sx) return(all(x==1))
    }
    return(TRUE) 
  }
  #
  nb  <- ceiling((to-from)/min(widths))
  # ensure that a solution exists
  if (!equal(nb, round(nb))) stop("`to-from` must be a multiple integer of `min(widths)` but is %f", nb)
  maxit1 <- 1000
  while(maxit1>0) { # repeat{
    # create breaks
    maxit2 <- 1000
    while(maxit2>0) { # repeat {
      breaks <- from+c(0, cumsum(sample(widths, nb, replace=TRUE)))
      breaks <- breaks[breaks<=to]
      if (max(breaks)<to) breaks <- c(breaks, to)
      ws     <- diff(breaks)
      if (any(equal(ws[length(ws)], widths))) break
      maxit2 <- maxit2-1
    }
    fail(maxit2==0, match.call())
    if (max)
      # create probs
      probs <- ddiscrete(ws)
    pw    <- probs/ws
    fracs <- fractions(pw)
    txt   <- strsplit(attr(fracs, "fracs"), "/", fixed=TRUE)
    denom <- sapply(txt, function(e) { if (length(e)>1) as.integer(e[2]) else 1})
    if ((max(denom)<=dmax) && unique_max(pw, tol=1e-6)) break
    maxit1 <- maxit1-1
  }  
  fail(maxit1==0, match.call())
  n <- lcmval(denom)*probs
  for (i in 1:1000) {
    if (all(equal(i*n, round(i*n)))) break
  }
  stopifnot(i<1000)
  list(breaks=breaks, n=i*n, decimal=is_ter(denom))
}
