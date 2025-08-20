#' @rdname as_ts
#' @title Convert ts_data Object to Time Series
#' @description Transforms a \code{ts_data} object into a standard R \code{ts} (time series) object.
#'
#' @param ts A \code{ts_data} object containing the time series values and structure.
#'
#' @return A \code{ts} object representing the same time series as the input \code{ts_data}.
#'
#' @details The function preserves the original time indices and frequency of the input \code{ts_data}. 
#' It calculates the \code{deltat} based on the time vector \code{t} in \code{ts_data}.
#'
#' @importFrom stats ts
#' @export
#'
#' @examples
#' # Create a ts_data object with a linear trend
#' ts_obj <- ts_data(12, trend.coeff = c(sample(0:10, 1), sample(1 + (1:10)/20, 1)))
#' 
#' # Convert to standard ts object
#' ts_standard <- as_ts(ts_obj)
as_ts <- function (ts) {
  stopifnot("ts_data" %in% class(ts))
  n <- length(ts$t)
  ts(data=ts$xt, start=c(1, ts$s[1]), end=c(n%/%max(ts$s), ts$s[n]), deltat=(ts$t[n]-ts$t[1])/n, frequency=max(ts$s))
}
