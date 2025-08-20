#' sumofsquares1
#' 
#' Decomposes an integer `n2` into a sum of squared integers (\eqn{n2 = \sum_{i=1}^{nobs} x_i^2}).  
#' If `n` is not `NA` then it is ensured that \eqn{\sum_{i=1}^{nobs} x_i = 0}. 
#' Note if `nobs<=10` then the following data sets are available: 
#' * \code{sos100=sumofsquares(100, 10, zerosum=TRUE, maxt=Inf)}, 
#' * \code{sos200=sumofsquares(200, 10, zerosum=TRUE, maxt=Inf)}, 
#' * \code{sos400=sumofsquares(400, 10, zerosum=TRUE, maxt=Inf)}, 
#' * \code{sos800=sumofsquares(800, 10, zerosum=TRUE, maxt=Inf)}, and 
#' * \code{sos1000=sumofsquares(100, 10, zerosum=TRUE, maxt=Inf)} 
#' 
#' @param n2 integer: number to decompose as sum of squares
#' @param nobs integer: length of return values 
#' @param n integer: additional sum condition (default: `0`)
#' @param x numeric: vector of `nobs` starting values (default: `runif(nobs)`)
#' @param maxit integer: maximal number of iterations
#' @md
#' @return A integer vector of length `nobs`.
#' @importFrom stats optim
#' @export
#'
#' @examples
#' sumofsquares1(100, 20)
#' sumofsquares1(100, 20)
sumofsquares1 <- function (n2, nobs=10, n=0, x=runif(nobs), maxit=1000) {
  optfun <- function(x, n2, n) { 
    err <- if (is.finite(n)) abs(sum(x)-n) else 0
    err+abs(sum(x^2)-n2)
  }
  #
  sx <- x
  suppressWarnings({
    n2 <- as.integer(n2)  
    n  <- as.integer(n)    
  })
  stopifnot(all(is.finite(c(n2, x))))
  start <- 0
  repeat { # make a restart
    start <- start+1
    # find a continuous solution
    x    <- sample(sx, length(x), replace=TRUE)
    opt  <- optim(x, method="BFGS", n2=n2, n=n, fn=optfun, control=list(maxit=maxit))
    x    <- as.integer(round(opt$par))
    fx   <- optfun(x, n2, n)
    index <- sample(nobs, maxit, replace=TRUE)
    one   <- sample(c(-1,1), maxit, replace=TRUE)
    # find an integer solution
    for (i in 1:maxit) {
      y           <- x
      y[index[i]] <- y[index[i]]+one[i]
      fy <- optfun(y, n2, n)
      if (fy<=fx) {
        x  <- y
        fx <- fy
      }
      if (fx==0) break
    }
    if (fx==0) break
  }
  opt$counts['start'] <- start
  opt$counts['maxit'] <- i
  structure(x, opt=opt$counts)
}