#' @rdname hyper_param
#' @title Parameters for Hypergeometric Distributions
#' @description Generates a data frame with potential values for `m`, `n` and `k`. If `hyper2` is `FALSE` then the 
#' parametrization of [stats::dhyper()] is used, otherwise `n+m`, `m` and `k` is used and transformed to `m`, `n` and `k`.
#' In accordance with specific conditions it holds that:
#' \itemize{
#' \item if `length(mean)==1` and it's an integer, it signifies the desired number of digits for the mean
#' \item if `mean` is set to `NA` (the default), all means are permissible
#' \item when `length(mean) > 1`, the product \eqn{k*m/(n+m)} must be one of the valid means
#' \item the same rules apply to `sd`
#' }
#' The parameters `norm`, `pois` and `binom` can take on the values `NA`, `TRUE`, `FALSE`, 
#' or be defined as a function of the format: `function(m, n, k)`. 
#' These values determine which `(m, n, k)` combinations are eligible:
#' \itemize{
#' \item for `NA`, all combinations of `(m, n, k)` are acceptable
#' \item if specified as a function, only those combinations for which the function evaluates to `TRUE` are considered valid
#' \item if set to `TRUE`, combinations are accepted only if they satisfy either the condition \eqn{k*m/(m+n)*(1-m/(m+n))>=9} 
#' (for `norm`, indicating a normal distribution approximation), the conditions \eqn{k/(n+m) < 0.05}, \eqn{m/(n+m) < 0.05} and \eqn{k>10} 
#' (for `pois`, implying a Poisson distribution approximation) and the condition \eqn{k/(n+m) < 0.05} (for `binom`, 
#' implying a binomial distribution approximation)
#' \item if set to `FALSE`, the approximations should not hold for any combination.
#' }
#' Please be aware that there is no guarantee that the resulting data frame will include a valid solution.
#' 
#' @param m	numeric: the number of white balls in the urn
#' @param n	numeric: the number of black balls in the urn
#' @param k	numeric: the number of balls drawn from the urn, hence must be in \eqn{0, 1, ..., m+n}
#' @param mean integer or numeric: number of digits the mean should have
#' @param sd integer or numeric: number of digits the standard deviation should have
#' @param norm logical or function: normal approximation possible
#' @param pois logical or function: poisson approximation possible
#' @param binom logical or function: binomial approximation possible
#' @param tol numeric: the tolerance for numerical comparison (default: `1e-6)
#' @param hyper2 logical: should the standard R parametrization `(m, n, k)` be used or `(n+m, m, k)`?
#'
#' @return A data frame with possible the choices of `n` , `p`, `mean` and `sd`. 
#' @export
#'
#' @examples
#' hyper_param(7:14, 1:13, 3:10, norm=FALSE, pois=FALSE, binom=FALSE, hyper2=TRUE)
hyper_param <- function (m, n, k, mean = NA, sd = NA, norm = NA, pois = NA, binom=NA, tol=1e-6, hyper2=FALSE) {
  if (hyper2) {
    g <- r <- expand.grid(m=m, n=n, k=k)
    g[,1] <- r[,2]
    g[,2] <- r[,1]-r[,2]
  } else {
    g <- expand.grid(m=m, n=n, k=k)
  }
  g <- g[apply(g,1, function(v) { all(v>0, v[3]<=v[1]+v[2]) }),]
  N <- g[,1]+g[,2] 
  g <- cbind(g, N=N,
             mean=g[,'k']*g[,'m']/N, 
             var=g[,'k']*g[,'m']/N*(1-g[,'m']/N)*(N-g[,'k'])/(N-1))
  # check mean
  if (length(mean)==1) {
    cond <- if (is.na(mean)) rep(TRUE, nrow(g)) else equal(g[,'mean'], round(g[,'mean'], mean), tol=tol)
  } else {
    cond  <- equal(g[,'mean'], mean, outer=TRUE, tol=tol)
    amean <- mean
  }
  g    <- g[cond,]
  # check variance
  asd <- sqrt(g[,'var'])
  if (length(sd)==1) {
    cond <- if (is.na(sd)) rep(TRUE, nrow(g)) else equal(asd, round(asd, sd), tol=tol)
  } else {
    cond <- equal(asd, round(asd, sd), outer=TRUE, tol=tol)
  }
  g    <- g[cond,]
  # check norm
  if (is.function(norm)) cond <- norm(g[,1:3])
  if (is.logical(norm)) { 
    if (is.na(norm)) {
      cond <- rep(TRUE, nrow(g))
    } else {
      cond <- g[,'k']*g[,'m']/g[,'N']*(1-g[,'m']/g[,'N'])
      cond <- if (norm) (cond>=9) else (cond<9)  
    }
  }
  g    <- g[cond,]
  # check pois
  if (is.function(pois)) cond <- pois(g[,1:3])
  if (is.logical(pois)) { 
    if (is.na(pois)) {
      cond <- rep(TRUE, nrow(g))
    } else {
      cond <- (g[,'k']/g[,'N']<0.05) & (g[,'m']/g[,'N']<0.05) & (g[,'k']>10)
      cond <- if (pois) cond else !cond
    }
  }
  # check binom
  if (is.function(binom)) cond <- binom(g[,1:3])
  if (is.logical(binom)) { 
    if (is.na(binom)) {
      cond <- rep(TRUE, nrow(g))
    } else {
      cond <- (g[,'k']/g[,'N']<0.05) 
      cond <- if (binom) cond else !cond
    }
  }
  g   <- g[cond,]
  data.frame(m=g[,'m'], n=g[,'n'], k=g[,'k'], mean=g[,'mean'], sd=sqrt(g[,'var']))
}
