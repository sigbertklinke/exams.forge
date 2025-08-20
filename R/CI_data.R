#' @importFrom stats qt
#' @rdname CImu_data
#' @aliases dcimu
#' @title Confidence Intervals for a Population Mean
#' @description
#' Computes confidence intervals for a population mean (\code{mu}) using either 
#' supplied data or data generated from a normal distribution with specified 
#' parameters. The function calculates key statistics such as the sample mean, 
#' standard deviation, and confidence intervals at user-defined confidence levels. 
#' Results are returned as a structured list containing observed and/or theoretical 
#' values, interval endpoints, and related measures.
#'
#' @param x numeric vector of observed data. If \code{NULL}, data are simulated.
#' @param n integer: sample size (if \code{n < 1}, defaults to \code{5}).
#' @param xbar numeric: sample mean (computed from \code{x} if \code{NULL}).
#' @param sd numeric: sample standard deviation (computed from \code{x} if \code{NULL}).
#' @param conf.level numeric vector of confidence levels (default: \code{c(0.9, 0.95, 0.99)}).
#' @param mu numeric: true population mean (used for simulation if \code{x} is \code{NULL}).
#' @param sigma numeric: population standard deviation(s) (used for simulation if \code{x} is \code{NULL}).
#'
#' @return A list containing:
#' \item{a}{upper-tail probability \code{1 - (1 - conf.level) / 2}}
#' \item{n}{sample size}
#' \item{xbar}{sample mean}
#' \item{mu}{theoretical mean (if provided)}
#' \item{sd}{sample standard deviation}
#' \item{sigma}{theoretical standard deviation (if provided)}
#' \item{df}{degrees of freedom (if using a \emph{t} distribution)}
#' \item{q}{critical value(s) from the normal or \emph{t} distribution}
#' \item{ss}{standard deviation used in calculations (\code{sd} or \code{sigma})}
#' \item{e}{margin of error (half-width of the interval)}
#' \item{l}{interval length}
#' \item{v}{confidence interval endpoints}
#'
#' @export
#'
#' @examples
#' # Using observed data
#' x <- rnorm(100)
#' CImu_data(x, conf.level = 0.95)
#'
#' # Simulating data internally
#' CImu_data(n = 100, conf.level = 0.95, mu = 0, sigma = 1)
CImu_data <- function(x=NULL, n=length(x), xbar=NULL, sd=NULL, conf.level=c(0.9, 0.95, 0.99), mu=NULL, sigma=NULL) {
  if (is.null(x)) {
    if (n<1) n <- 5
    if (length(sigma)>1) sigma <- sample(sigma, 1)
    if (length(mu)>1) mu <- sample(mu, 1)
    x <- rnorm(n, mean=mu, sd=sigma)
  } 
  if (is.null(xbar)) xbar <- mean(x)
  if (is.null(sd)) sd <- sd(x)
  ret <- list(a=1-(1-conf.level)/2, n=length(x), xbar=xbar, mu=mu, sd=sd, sigma=sigma, df=NULL)
  if (is.null(sigma)) {
    ret$q  <- qnorm(ret$a)
    ret$ss <- ret$sd
  } else {
    ret$df <- length(x)-1
    ret$q  <- qt(ret$a, ret$df)
    ret$ss <- ret$sigma
  }
  names(ret$q) <- conf.level
  ret$e <- ret$q*ret$ss/sqrt(ret$n)
  ret$l <- 2*ret$e
  ret$v <- xbar+c(-ret$e, ret$e)
  ret
}
#' @rdname CImu_data
#' @export dcimu
# dcimu <- function(...){
#  CImu_data(...)}
dcimu <- CImu_data
