#' @rdname all_different
#' @aliases values_different all_different
#' @title Difference Testing Functions
#'
#' @description These functions determine whether numeric values are sufficiently different from each other based on a specified tolerance.
#' - `all_different`: Checks if all differences between entries in a numeric object exceed a given tolerance.
#' - `values_different`: Adds candidate values to a set of initial values only if they are sufficiently different from all existing values.
#'
#' @param obj Numeric vector, matrix, or data frame to test for differences. Non-numeric inputs will be coerced to numeric if possible.
#' @param values Numeric vector of initial values.
#' @param candidates Numeric vector of candidate values to add if sufficiently different.
#' @param tol Numeric scalar specifying the minimum allowable difference between values.
#'
#' @return
#' - `all_different`: Logical (`TRUE` if all differences exceed `tol`, `FALSE` otherwise).
#' - `values_different`: Numeric vector with original `values` plus any `candidates` that meet the difference criterion.
#'
#' @importFrom stats dist
#' @export
#'
#' @examples
#' # Check if all values are sufficiently different
#' x <- runif(10)   # 10 random values between 0 and 1
#' all_different(x, tol = 0.01)
#' all_different(x, tol = 0.5)
#'
#' # Add sufficiently different candidate values
#' starting_values <- c(0.1, 0.5, 0.9)
#' candidates <- c(0.15, 0.4, 0.8, 1.2)
#' values_different(starting_values, candidates, tol = 0.2)
all_different <- function(obj, tol) {
  stopifnot(!missing(tol))
  if (!is.data.frame(obj)) obj <- as.data.frame(obj)
  all(rowSums(as.matrix(dist(obj, "maximum"))<tol)<2)
}

#' @rdname all_different
#' @export
values_different  <- function (values, candidates, tol=0.1) {
  for (v in candidates) {
    if (all(abs(values-v)>abs(tol))) values <- c(values, v)
  }
  values
}
