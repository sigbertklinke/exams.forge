#' @rdname sample_size_freq
#' @title Sample Size Consistency Checker
#' @aliases dnsizefreq
#' @description Checks if a vector of possible sample sizes and relative frequencies create integer absolute frequencies.
#' @param n numeric: vector of sample size(s) to check
#' @param f numeric: vector of relative frequencies
#' @param which numeric: if several `n`'s are possible then `which` is returned (default: `NA` = choose a random one)
# @param ... further arguments for the alias
#'
#' @return One sample size.
#' @export
#'
#' @examples
#' f <- ddiscrete(runif(5), unit=100)
#' sample_size_freq(seq(10, 200, 1), f)
#' sample_size_freq(seq(10, 200, 1), f, which=200)
sample_size_freq <- function(n, f, which=NA) {
  n <- n[apply(outer(n, f), 1, all_integer)]
  stopifnot(length(n)>0)
  if (length(n)==1) return(n)
  if (is.na(which)) return(sample(n, 1))
  i <- as.integer(which)
  if (i<1) i <- 1
  if (i>length(n)) i <- length(n)
  n[i]
}

#' @rdname sample_size_freq
#' @export
# dnsizefreq <- function(...){
#  sample_size_freq(...)}
dnsizefreq <- sample_size_freq
