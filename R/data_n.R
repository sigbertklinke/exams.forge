#' @rdname data_n
#' @aliases data_n25
#' @aliases data_nsq
#' @aliases dn
#' @aliases dn25
#' @aliases dnsq
#' @title Number of Observations
#' @description Generates a sequence of sample sizes in a range from `min=5` to a `max`:
#' * whose root is an integer (`data_nsq`), and
#' * that are divisible only by 2 and 5 (`data_n25`)
#'
#' @param max integer: maximum sample size
#' @param min integer: minimum sample size (default: `5`)
#'
#' @return A sequence of integers.
#' @export
#'
#' @examples
#' data_n(10)
#' data_nsq(1000)
#' data_n25(1000)
data_n <- function(max, min=5) {
  ceiling(min):floor(max)
}
  
#' @rdname data_n
#' @export
data_nsq <- function(max, min=5) {
  nobs <- sqrt(round(c(min, max)))
  (ceiling(nobs[1]):floor(nobs[2]))^2
}

#' @rdname data_n
#' @export
data_n25 <- function(max, min=5) {
  nobs <- round(min):round(max)
  nobs[divisor_25(nobs)]
}

#' @rdname data_n 
#' @export 
# dn <- function(...){
#  data_n(...)}
dn <- data_n

#' @rdname data_n 
#' @export 
# dn25 <- function(...){
#  data_n25(...)}
dn25 <- data_n25

#' @rdname data_n 
#' @export 
# dnsq <- function(...){
#  data_nsq(...)}
dnsq <- data_nsq
