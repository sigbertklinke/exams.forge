library("knitr")
library("latex2exp") # für TeX(...)

the      <- new.env(parent = emptyenv())
the$dist <- list()

craft <- function(name, ..., .overwrite=FALSE) {
  browser()
  l <- substitute(...())
  l$name <- name
  ln     <- names(l)
  if (.overwrite || is.null(the$dist[[name]])) {
    the$dist[[name]] <- l[!(duplicated(ln)|ln=='')]
    return(name)
  }
  stop(sprintf("Distribution template '%s' exist already", name))
}

#' @rdname Distribution
#' @aliases compute_cdf 
#' @aliases compute_pmdf
#' @aliases compute_probability
#' @aliases point_probability
#' @aliases pprob
#' @aliases is_distribution
#' @title Class Distribution
#' @description Holds an univariate distribution including its parameters. The name of the distribution is used to determine the right use of the function, for example 
#' the function for quantiles: \code{paste0("q", name)}. Usually the full name has to be used; some abbreviated names are possible
#' \itemize{
#' \item{\code{binom}} binomial distribution, parameters: \code{size}, \code{prob}
#' \item{\code{hyper}} hypergeometric distribution, parameters: \code{m}, \code{n}, \code{k}
#' \item{\code{geom}} geometric distribution, parameters: \code{prob}
#' \item{\code{pois}} Poisson distribution, parameters: \code{lambda}
#' \item{\code{unif}} continuous uniform  distribution, parameters: \code{min}, \code{max}
#' \item{\code{dunif}} discrete uniform  distribution, parameters: \code{min}, \code{max}
#' \item{\code{dunif2}} continuous uniform  distribution, parameters: \code{min}, \code{max}
#' \item{\code{exp}} exponential distribution, parameter: \code{rate}
#' \item{\code{norm}} normal distribution, parameters: \code{mean}, \code{sd}
#' \item{\code{lnorm}} log-normal distribution, parameters: \code{meanlog}, \code{sdlog}
#' \item{\code{t}} Student t distribution, parameter: \code{df}
#' \item{\code{chisq}} chi-squared distribution, parameter: \code{df}
#' \item{\code{f}} F distribution, parameters: \code{df1},  \code{df2}
#' }
#' 
#' The following functions exists for \code{disributions}:
#' \itemize{
#' \item{\code{distribution}} creates a distribution with name `name` and parameters
#' \item{\code{quantile}} computes the quantiles of a distribution using `paste0('q', name)`
#' \item{\code{cdf}} computes the cumulative distribution function of a distribution using `paste0('p', name)`
#' \item{\code{pmdf}} computes the probability mass/density function of a distribution using `paste0('d', name)`
#' \item{\code{prob}} computes the probability for a interval between `min` and `max` (`max` included, `min` excluded)
#' \item{\code{prob1}} computes the point probability f
#' \item{\code{is.distribution}} checks if `object` is distribution object. If `name` is given then it checks wether the distribution type is the same  
#' \item{\code{toLatex}} generates a LaTeX representation of the distribution an its parameter
#' }
#' @param name character: name of the distribution type
#' @param ... further named distribution parameters
#' @param discrete logical: Is distribution discrete? (default: \code{NA}) 
#'
#' @return a distribution object
#' @export
#'
#' @examples
#' d <- distribution("norm", mean=0, sd=1)
#' quantile(d)
#' quantile(d, c(0.025, 0.975))
#' cdf(d, 0)
#' is.distribution(d)
#' is.distribution(d, "t")
#' toLatex(d)
#' # see the LaTeX names
#' data(distributions)
#' distributions
distribution <- function(name, ...) {
  #browser()
  d     <- the$dist[[name]]
  if (is.null(d))     
    stop(sprintf("Distribution template '%s' dos not exist", name))
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('',length(args))
  for (narg in nargs) if (narg!='') d[[narg]] <- args[[narg]]
  nargs <- names(d)
#  if (is.null(nargs)) nargs <- rep('',length(args))
  for (narg in nargs) 
    d[[narg]] <- eval(d[[narg]], d)
  for (narg in nargs) { 
    if (mode(d[[narg]])=="character") d[[narg]] <- knit(text=d[[narg]], envir=list2env(d))
  }
  d$call <- match.call()
  if (is.null(d$median) && !is.null(d$qdist))     d$median <- substitute(qdist(0.5))
  if(!is.null(d$toLatex) && is.null(d$toString)) d$toString <- gsub("[\\\\\\\\]", "", d$toLatex)
  if(!is.null(d$toLatex)  && is.null(d$toMath))  d$toMath <- TeX(d$toLatex)
  structure(d, class=c("distribution", class(d)))
}

property <- function(name=NULL, which=NULL) {
  ret <- if (is.null(name)) sapply(the$dist, function(e) { names(e) }) else names(the$dist[[name]])
  sort(unique(ret))
}

# new
print.distribution <- function(x, ...) { deparse(x$call) }

# new
summary.distribution <- function(object, ...) {
  cat(deparse(object$call[[2]]), "\n")
  str(object$a, give.attr = FALSE)
}

# old: quantile.distribution <- function(x, probs=seq(0, 1, 0.25), ...)
ddist <- pmdf <- function(d, x)                          { d$ddist(x) }
pdist <- pmdf <- function(d, q)                          { d$pdist(q) }
qdist <- quantile.distribution <- function(d, p=(0:4)/4) { d$qdist(p) }
rdist <- quantile.distribution <- function(n)            { d$rdist(n) }

# old: toLatex.distribution <- function(object, name=NULL, param=NULL, digits=4, ...) 
toLatex.distribution  <- function(object, ...) { d$toLatex } 
toString.distribution <- function(object, ...) { d$toString }
toMath <- function(object, ...)                { d$toMath }


E     <- function(d) { d$expectation }
Var   <- function(d) { d$variance    }
ifint <- function(x, vec, res) { res[1+findInterval(x, vec)] }

# new:
dcategorical <- function(x)  { index <- which(outcome==x); if (length(index)==1) prob[index] else 0 }
rcategorical <- function(n)  { sample(outcome, n, replace=TRUE) }

