#' @title Midpoint-Based Data Creation for a Histogram
#' @rdname histx
#' @aliases gen_mid
#' @aliases dhistx
#' @description Given the breaks and the number of observations, a data set is generated with [stats::runif()], using 
#' the class mids: \eqn{x_i = class\_mid_j + alpha*class\_width_j/2}. The default `alpha=0.99` ensures that 
#' generated observations do not lie on the class borders.
#'
#' @param breaks numeric: class borders
#' @param n numeric: number of observations in each class
#' @param alpha numeric: how far the generated observations can be away from the class mids (default: \code{0.99})
#'
#' @md
#' @return The generated data set.
#' @export
#'
#' @examples
#' breaks <- sort(sample(seq(0.1, 0.9, by=0.1), 4))
#' bins   <- length(breaks)-1
#' n      <- rmultinom(1, size=100, prob=ddiscrete(runif(bins)))
#' histx(breaks, n) 
histx <- function (breaks, n, alpha=0.99) {
  nb <- length(breaks)
  stopifnot(nb==1+length(n))
  xm <- (breaks[-1]+breaks[-nb])/2
  xd <- diff(breaks)
  x <- numeric(0)
  for (i in 1:length(n)) {
    x <- c(x, runif(n[i], min=xm[i]-alpha*xd[i]/2, max=xm[i]+alpha*xd[i]/2))
  }
  sample(x, length(x))
}

#' @rdname histx
#' @export
# gen_mid <- function(...){
#  histx(...)}
gen_mid <- histx

#' @rdname histx
#' @export
# dhistx <- function(...){
#  histx(...)}
dhistx <- histx