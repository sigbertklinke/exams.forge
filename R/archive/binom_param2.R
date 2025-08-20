#' @rdname binom_param
#' @title Binomial Parameters
#' @description Generates a data frame with potential values for `size` and `prob`, 
#' and is subjected to specific conditions:
#' \itemize{
#' \item If `length(mean) == 1` and it's an integer, it signifies the desired number of digits for the mean.
#' \item If `mean` is set to `NA` (the default), all means are permissible.
#' \item When `length(mean) > 1`, the product `size * prob` must be one of the valid means. 
#' \item The same rules applies to `sd`.
#' }
#' The parameters `norm` and `pois` can take on values of `NA`, `TRUE`, `FALSE`, 
#' or be defined as a function in the format: `function(size, prob)`. 
#' These values determine which `(size, prob)` combinations are eligible:
#' \itemize{
#' \item For `NA`, all combinations of `(size, prob)` are acceptable.
#' \item If specified as a function, only those combinations for which the function returns `TRUE` are considered valid.
#' \item If set to `TRUE`, combinations are accepted only if they satisfy either the condition `size * prob * (1 - prob) > 9` 
#' (for `norm`, indicating a normal distribution approximation), or the conditions `prob < 0.05` and `n > 10` 
#' (for `pois`, implying a Poisson distribution approximation).
#' \item If set to `FALSE`, the approximations should not hold for any combination.
#' }
#' Please be aware that there is no guarantee that the resulting data frame will include a valid solution.
#' 
#' @param n integer: vector number of observations
#' @param p numeric: vector of probabilities
#' @param mean integer or numeric: number of digits the mean should have
#' @param sd integer or numeric: number of digits the standard deviation should have
#' @param norm logical or function: normal approximation possible
#' @param pois logical or function: poisson approximation possible
#' @param tol numeric: the tolerance for numerical comparison (default: `1e-6)
#'
#' @return a data frame with possible choices of `n` , `p`, `mean` and `sd` 
#' @export
#'
#' @examples
#' binom_param(1000:50000, (5:25)/100, 0, 0)
#binom_param <- function(n, p, mean=NA, sd=NA, norm=NA, pois=NA) {
#  if (is.na(mean)) {
#    mean <- ceiling(mean(log10(c(min(n)*min(p), max(n)*max(p)))))
#    if (mean>0) mean <- 0 else mean <- 1-mean
#  }
#  mean <- round(mean)
#  if (is.na(sd)) sd <- mean
#  res <- list(n=numeric(0), p=numeric(0), mean=numeric(0), sd=numeric(0))
#  for (i in 1:length(p)) {
#    m   <- n*p[i]
#    s   <- sqrt(n*p[i]*(1-p[i]))
#    ind <- which(has_digits(m, mean) & has_digits(s, sd)) 
#    if (length(ind)) {
#      res$n <- c(res$n, n[ind])
#      res$p <- c(res$p, rep(p[i], length(ind)))
#      res$mean <- c(res$mean, m[ind])
#      res$sd   <- c(res$sd, s[ind])
#    }
#  }
#  res <- as.data.frame(res)
#  if (isTRUE(norm))  res <- res[res$sd>9,]
#  if (isFALSE(norm)) res <- res[res$sd<9,]
#  if (isTRUE(pois))  res <- res[(res$n>10) & (res$p<0.05),]
#  if (isFALSE(pois)) res <- res[(res$n<10) | (res$p>0.05),]
#  res
#}
binom2norm <- function(size, prob, c=9) { 
  if (length(c)==1) return(size*prob*(1-prob)>=c)
  if (length(c)==2) return((size * prob > c[1]) & (size * (1 - prob) > c[2]))
  stop("'c' must have a length of either one or two")
}
binom2pois <- function(size, prob, c=c(10, 0.05)) { 
  if (length(c)==1) return(size*prob<c)
  if (length(c)==2) return((size>c[1]) & (prob<c[2]))   
  stop("'c' must have a length of either one or two")
}

binom_param <- function (n, p, mean = is_terminal, sd = is_terminal, norm = binom2norm, pois = binom2pois, ...) {

  mean <- match.fun(mean)
  sd   <- match.fun(sd)
  if (is.logical(norm)) {
    norm <- function(n, p, ...) { rep(TRUE, length(n)) }
    if (isTRUE(norm))  norm <- function(n, p, ...) { binom2norm(n, p, ...) }
    if (isFALSE(norm)) norm <- function(n, p, ...) { !binom2norm(n, p, ...) }
  } else {
    norm <- match.fun(norm)
  }
  g <- expand.grid(n=n,p=p)
  g$mean <- g[,1]*g[,2]
  g$sd   <- sqrt(g[,1]*g[,2]*(1-g[,2]))
  g$norm <- norm(g$n, g$p, ...)
  g$pois <- pois(g$n, g$p)
  g <- g[mean(g$mean) & sd(g$sd),]
  g <- g[sd(g$sd),]
  g
}
