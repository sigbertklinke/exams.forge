#' Generate Break Points for Equidistant or Quantile Bins
#'
#' Creates a numeric vector of break points for the given data `x`. 
#' The resulting breaks define bins that are either equidistant (fixed width) 
#' or non-equidistant (quantile-based). If `width` is not specified, it defaults 
#' to \code{diff(pretty(x))[1]}. The `probs` argument can either be a single 
#' integer, specifying the number of quantiles, or a numeric vector of probabilities 
#' in the interval \eqn{[0, 1]}. 
#' 
#' @param x numeric vector: the data to compute breaks for.
#' @param width numeric, optional: desired bin width (default: \code{NULL}, auto-calculated).
#' @param probs numeric, optional: number of quantiles (single integer) or 
#'   vector of probabilities in \eqn{[0, 1]} for non-equidistant bins (default: \code{NULL}).
#'
#' @return A numeric vector containing the break points.
#'
#' @details
#' If `probs` is used, break points are rounded to the nearest multiple of `width`. 
#' Duplicates are removed, and the range is extended if necessary to include the 
#' full range of `x`.
#'
#' @aliases add_breaks dbreaks
#' @export
#' @md
#'
#' @examples
#' x <- rnorm(100, mean = 1.8, sd = 0.1)
#' breaks(x)                # equidistant bins
#' breaks(x, width = 0.1)   # custom width bins
#' breaks(x, width = 0.1, probs = 4)  # quantile-based bins
breaks <- function(x, width=NULL, probs=NULL) {
  if (is.null(width)) width <- diff(pretty(x))[1]
  if (!is.null(probs)) { # not equidistant
    if (length(probs)==1) probs <- seq(0, 1, by=1/probs)
    if (min(probs)!=0) probs <- c(0, probs)
    if (max(probs)!=1) probs <- c(probs, 1)    
    ret <- round(quantile(x, probs)/width)*width
  } else { # equidistant
    bmin <- floor(min(x)/width)*width
    bmax <- ceiling(max(x)/width)*width
    ret <- seq(bmin, bmax, by=width)
  }
  if (anyDuplicated(ret)) ret <- unique(ret)
  if (min(ret)>min(x)) ret <- c(ret[1]-width, ret)
  if (max(ret)<max(x)) ret <- c(ret, ret[length(ret)]+width)

  #
  ret
}

#' @rdname breaks
#' @export 
# add_breaks <- function(...){breaks (...)}
add_breaks <- breaks

#' @rdname breaks
#' @export 
# dbreaks <- function(...){breaks(...)}
dbreaks <- breaks
