#' @rdname binom_param
#' @title Generate Valid Binomial Parameters
#' @description Creates a data frame of possible combinations of `n` (number of trials) 
#' and `p` (probabilities) that satisfy specified constraints on the mean, standard deviation, 
#' and approximation conditions for normal or Poisson distributions.
#' 
#' The function applies the following rules:
#' \itemize{
#'   \item If `length(mean) == 1` and it is an integer, it specifies the number of digits 
#'   to which the mean should be rounded.
#'   \item If `mean = NA` (default), all mean values are allowed.
#'   \item If `length(mean) > 1`, only combinations where `n * p` equals one of the specified means are retained.
#'   \item The same logic applies to `sd` for the standard deviation.
#' }
#' 
#' The `norm` and `pois` arguments can be logical, `NA`, or a custom function of the form `function(n, p)`. 
#' They control which `(n, p)` pairs are considered valid:
#' \itemize{
#'   \item `NA` allows all combinations.
#'   \item A function returns `TRUE` for valid combinations and `FALSE` for invalid ones.
#'   \item `TRUE` enforces standard approximation rules: 
#'     \itemize{
#'       \item `norm`: `n * p * (1 - p) > 9` (normal approximation condition)
#'       \item `pois`: `n > 10 & p < 0.05` (Poisson approximation condition)
#'     }
#'   \item `FALSE` excludes combinations that meet the approximation condition.
#' }
#' 
#' Note: The resulting data frame may be empty if no combinations meet all criteria.
#' 
#' @param n integer vector of trial counts
#' @param p numeric vector of probabilities
#' @param mean numeric or integer specifying required mean digits or specific mean values
#' @param sd numeric or integer specifying required standard deviation digits or specific sd values
#' @param norm logical, `NA`, or function: restricts combinations to those valid for normal approximation
#' @param pois logical, `NA`, or function: restricts combinations to those valid for Poisson approximation
#' @param tol numeric: tolerance for numerical comparisons (default: `1e-6`)
#'
#' @return A data frame with columns `n`, `p`, `mean`, and `sd` representing valid parameter combinations.
#' @export
#'
#' @examples
#' binom_param(1000:50000, (5:25)/100, mean = 0, sd = 0)
binom_param <- function (n, p, mean = NA, sd = NA, norm = NA, pois = NA, tol=1e-6) {
  g <- expand.grid(n=n,p=p)
  g <- cbind(g, e=g[,1]*g[,2], v=g[,1]*g[,2]*(1-g[,2]))
  # check mean
  if (length(mean)==1) {
    cond <- if (is.na(mean)) rep(TRUE, nrow(g)) else equal(g[,3], round(g[,3], mean), tol=tol)
  } else {
    cond <- equal(g[,3], mean, outer=TRUE, tol=tol)
    amean <- mean
  }
  g    <- g[cond,]
  # check variance
  asd <- sqrt(g[,4])
  if (length(sd)==1) {
    cond <- if (is.na(sd)) rep(TRUE, nrow(g)) else equal(asd, round(asd, sd), tol=tol)
  } else {
    cond <- equal(asd, round(asd, sd), outer=TRUE, tol=tol)
  }
  g    <- g[cond,]
  # check norm
  if (is.function(norm)) cond <- norm(g[,1:2])
  if (is.logical(norm)) { 
    if (is.na(norm)) {
      cond <- rep(TRUE, nrow(g))
    } else {
      cond <- if (norm) (g[,4]>9) else (g[,4]<9)  
    }
  }
  g    <- g[cond,]
  # check pois
  if (is.function(pois)) cond <- pois(g[,1:2])
  if (is.logical(pois)) { 
    if (is.na(pois)) {
      cond <- rep(TRUE, nrow(g))
    } else {
      cond <- if (pois) ((g[,1]>10) & (g[,2]<0.05)) else  ((g[,1]<10) | (g[,2]>0.05))
    }
  }
  g   <- g[cond,]
  data.frame(n=g[,1], p=g[,2], mean=g[,3], sd=sqrt(g[,4]))
}
