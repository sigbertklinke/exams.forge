#' @rdname approx
#' @aliases approx_binom2norm
#' @aliases approx_clt2norm
#' @aliases approx_t2norm
#' @title Distribution Approximations
#' @description These functions check whether a normal approximation is appropriate for a given distribution.  
#' They return `TRUE` if the approximation condition is met, and `FALSE` otherwise.  
#' The threshold parameter `c` can be set directly or retrieved via `getOption()`.  
#' 
#' The functions apply the following rules:
#' 
#' * `t2norm`: `n > c` with default `c = 30`.
#' * `binom2norm`:  
#'   - If `type = "single"` (default), the approximation is valid if `size × prob × (1 - prob) > c`.  
#'   - If `type = "double"`, the approximation requires both `size × prob > c` and `size × (1 - prob) > c`, with default `c = 9`.
#' * `clt2norm`: `n > c` with default `c = 30`. Note that the existence of expectation and variance, required by the Central Limit Theorem, cannot be checked automatically.
#'
#' @param n integer: number of observations (for `t2norm` and `clt2norm`)
#' @param size integer: number of trials (for `binom2norm`)
#' @param prob numeric: probability of success on each trial (for `binom2norm`)
#' @param type character: approximation type, `"single"` or `"double"` (for `binom2norm`)
#' @param c numeric: threshold parameter for approximation (default via `getOption()` or a default value)
#'
#' @return logical: `TRUE` if the approximation is valid, `FALSE` otherwise
#' @export
#' @md
#'
#' @examples
#' # Check for 5 and 50 observations
#' t2norm(n = c(5, 50))
#' binom2norm(size = c(5, 50), prob = 0.5)
#' binom2norm(size = c(5, 50), prob = 0.5, type = "double")
t2norm <- function(n, c=getOption("distribution.t2norm", 30)) {
  (n>c)
}

#' @rdname approx
#' @export
binom2norm <- function(size, prob, c=getOption("distribution.binom2norm", 9), type=c("single", "double")) {
  type <- match.arg(type)
  if (type=="single") return(size*prob*(1-prob)>c)
  return((size*prob>c) & (size*(1-prob)>c))
}

#' @rdname approx
#' @export
clt2norm <- function(n, c=getOption("distribution.clt2norm", 30)) {
  (n>c)
}

#' @rdname approx
#' @export
# approx_binom2norm <- function(...){
#  binom2norm(...)}
approx_binom2norm <- binom2norm 

#' @rdname approx
#' @export
# approx_clt2norm <- function(...){
#  clt2norm(...)}
approx_clt2norm <- clt2norm

#' @rdname approx
#' @export
# approx_t2norm <- function(...){
#  t2norm(...)}
approx_t2norm <- t2norm
