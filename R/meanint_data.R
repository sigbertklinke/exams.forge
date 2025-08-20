#' @rdname meanint_data
#' @title Integer Observations and Mean
#' @description
#' The `meanint_data` function generates a set of integer observations with a specified integer mean. 
#' It takes the number of observations or `x` values  and an optional range parameter, `r`, that defines the permissible range 
#' of x values (defaulting to the range of `x`). Additional parameters are passed to the `mean` function.
#' The function employs a iterative process, adjusting individual observations to achieve an integer mean.
#' It uses a random selection approach, modifying a randomly chosen observation and checking if the resulting mean is closer to an integer. 
#' The process continues until the mean becomes an integer.
#' 
#' @param x numeric: number of observations or x values
#' @param r numeric: the range in which the x values allowed (default: `range(x)`)
#' @param ... further parameters given to `mean`
#'
#' @return A set of integer observations with an integer mean.
#' @export
#'
#' @examples
#' x <- meanint_data(10, c(1, 10))
#' mean(x)
meanint_data <- function(x, r=range(x), ...) {
  frac <- function(x) { abs(x-round(x)) }
  #
  if (length(x)==1) x <- runif(x, r[1], r[2])
  x  <- round(x)
  fx <- mean(x, ...)
  while(!has_digits(fx,0)) {
    xi <- x
    i  <- sample(length(x), 1)
    xi[i] <- x[i] + if (runif(1)<0.5) -1 else +1
    if ((xi[i]<r[1]) || (xi[i]>r[2])) xi[i] <- x[i] 
    fxi <- mean(xi, ...)
    if (frac(fxi)<frac(fx)) {
      x <- xi
      fx <- fxi
    }
  }
  x
}
