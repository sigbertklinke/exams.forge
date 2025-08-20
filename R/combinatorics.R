#' @rdname Combinatorics
#' @aliases combs
#' @aliases combo 
#' @aliases fact
#' @aliases factquot
#' @aliases binom
#' @title Combinatorics
#' @description
#' * `permutation` computes the number of permutations
#' * `variation` computes the number of variations with and without replication  
#' * `combination` computes the number of combinations with and without replication  
#' * `combinatorics` computes all combinatorics results for `k < n` and returns it as list of:
#' \describe{
#' \item{\code{permutation.n}}{\eqn{P(n)}}
#' \item{\code{permutation.k}}{\eqn{P(k)}}
#' \item{\code{permutation.nk}}{\eqn{P(n; k)}}
#' \item{\code{variation}}{\eqn{V(n;k)}}
#' \item{\code{variation.rep}}{\eqn{V^W(n;k)}}
#' \item{\code{combination}}{\eqn{K(n;k)}}
#' \item{\code{combination.rep}}{\eqn{K^W(n;k)}}
#' }
#' * `lfact` computes the natural logarithm of the factorial of a given number `n`
#' * `lfactquot` calculates the natural logarithm of the quotient of factorials
#' * `lbinom` computes the natural logarithm of the binomial coefficient, "`n` choose `k`"
#'
#'
#' @param n numeric: total number of elements
#' @param k numeric: number of elements to choose 
#' @param repl logical: with repetition (default: \code{FALSE})
#' @param ... numeric: further arguments for `lfactquot`
#'
#' @return A list.
#' @export
#'
#' @examples
#' permutation(8)
#' permutation(8, c(1,3,2,2))
#' combination(8, 4)
#' combination(8, 4, TRUE)
#' variation(8, 4)
#' variation(8, 4, TRUE)
#' combinatorics(8, 4)
combinatorics <- function(n, k) {
  ret <- list(permutation.n=permutation(n),
              permutation.k=permutation(k),
              permutation.nk=permutation(n, k),
              variation = variation(n, k),
              variation.rep = variation(n, k, TRUE),
              combination = combination(n, k),
              combination.rep = combination(n, k, TRUE)
             )
  attr(ret, "mindiff") <- min(diff(sort(unlist(ret))))
  ret
}

#' @rdname Combinatorics
#' @export
variation   <- function(n, k, repl=FALSE) { round(exp(if (repl) k*log(n) else lfactorial(n)-lfactorial(n-k))) }

#' @rdname Combinatorics
#' @export
combination <- function(n, k, repl=FALSE) { round(exp(if (repl) lchoose(n+k-1, k) else lchoose(n, k))) }

#' @rdname Combinatorics
#' @export
permutation <- function(n, k=rep(1, n)) { 
  stopifnot("groups have more than 'n' elements"=sum(k)<=n)
  if (sum(k)<n) warning("'sum(k)<n', one element group(s) added")
  round(exp(lfactorial(n)-sum(lfactorial(k)))) 
} 

#' @rdname Combinatorics
#' @export
lfact <- function(n) { lfactquot(n) }

#' @rdname Combinatorics
#' @export
lfactquot <- function(n, ...) {
  args <- unlist(list(...))
  ret  <- NULL
  if (length(args)==0) {
    fmax     <- n
    fraction <- rep(1, n)
    ret      <- c(ret, sprintf("%s!", fcvt(n)), paste0(fcvt(n:1), collapse=" \\cdot " ))
  } else {
    fmax     <- max(n, args)
    fraction <- rep(1, n)
    if (n<fmax) fraction <- c(fraction, rep(0, fmax-n)) 
    for (i in 1:length(args)) fraction[1:args[i]] <- fraction[1:args[i]]-1
    num   <- NULL
    denom <- NULL
    for (i in fmax:1) {
      if (fraction[i]>1)  num <- c(num, sprintf("%s^%i", fcvt(i), fraction[i])) 
      if (fraction[i]==1) num <- c(num, fcvt(i)) 
      if (fraction[i]==-1) denom <- c(denom, fcvt(i)) 
      if (fraction[i]< -1)  denom <- c(denom, sprintf("%s^%i", fcvt(i), -fraction[i])) 
    }  
    ret <- c(ret, sprintf("\\frac{%s!}{%s}", fcvt(n), paste0(fcvt(args), "!", collapse=" \\cdot " )))
    if (is.null(num)) num <- 1
    if (is.null(denom)) {
      ret <- c(ret, paste0(num, collapse=" \\cdot "))      
    } else {
      ret <- c(ret, sprintf("\\frac{%s}{%s}", paste0(num, collapse=" \\cdot "), paste0(denom, collapse=" \\cdot ")))
    }
  }
  res <- 0
  for (i in fmax:1) res <- res + fraction[i]*log(i)
  ret <- c(ret, fcvt(round(exp(res))))
  paste0(ret, collapse=" = ")
}

#' @rdname Combinatorics
#' @export
lbinom <- function(n, k) { sprintf("\\left(\\begin{array}{c} %s \\\\ %s \\end{array}\\right)", fcvt(n), fcvt(k)) }

#' @rdname Combinatorics
#' @export
# combo <- function(...){
#  combination(...)}
combo <- combination

#' @rdname Combinatorics
#' @export
# combs <- function(...){
#  combinatorics(...)}
combs <- combinatorics

#' @rdname Combinatorics
#' @export
# fact <- function(...){
#  lfact(...)}
fact <- lfact

#' @rdname Combinatorics
#' @export
# factquot <- function(...){
#  lfactquot(...)}
factquot <- lfactquot

#' @rdname Combinatorics
#' @export
# binom <- function(...){
#  lbinom(...)}
binom <- lbinom
