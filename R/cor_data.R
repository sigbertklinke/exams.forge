#' @rdname cor_data
#' @aliases dcorr
#' @title Generate Data with a Target Correlation
#' @description
#' Rearranges the order of `y` to construct a dataset with a target correlation `r` between `x` and `y`,
#' as defined by [stats::cor()].  
#' The marginal distributions of `x` and `y` are preserved, but the achieved correlation may deviate
#' from the target. The algorithm iteratively adjusts the ordering of `y`; increasing `maxit`
#' may improve accuracy if results are unsatisfactory.
#'
#' @param x numeric. Vector of `x` values.
#' @param y numeric. Vector of `y` values.
#' @param r numeric. Desired correlation.
#' @param method character. Correlation coefficient to compute. Options are `"pearson"` (default), 
#' `"kendall"`, or `"spearman"`.
#' @param maxit integer. Maximum number of iterations (default: `1000`).
#' @param ... Additional arguments passed to [stats::cor()].
#'
#' @return 
#' A two-column matrix with `x` and reordered `y`.  
#' An attribute `interim` stores a matrix of intermediate values, which depends on `method`:
#'
#' * **`pearson`**:  
#'   Rows include \eqn{x_i}, \eqn{y_i}, \eqn{x_i - \bar{x}}, \eqn{y_i - \bar{y}}, 
#'   squared deviations, and cross-products.  
#'
#' * **`kendall`**:  
#'   Rows include \eqn{x_i}, \eqn{y_i}, \eqn{p_i} (concordant pairs), and \eqn{q_i} (discordant pairs).  
#'
#' * **`spearman`**:  
#'   Rows include \eqn{x_i}, \eqn{y_i}, ranks of `x` and `y`, and squared rank differences.  
#'
#' In all cases, an additional column with row sums is appended.
#'
#' @examples
#' x <- runif(6)
#' y <- runif(6)
#' xy <- cor_data(x, y, r = 0.6)
#' cbind(x, y, xy)
#'
#' @export

cor_data <- function(x, y, r, method=c("pearson", "kendall", "spearman"), ..., maxit=1000) {
  stopifnot(length(x)==length(y))
  n      <- length(x)
  args   <- list(...)
  args$x <- x
  args$y <- y 
  args$method <- match.arg(method)
  rbest  <- do.call(cor, args)
  obest  <- 1:n
  i      <- 0
  while(i<maxit) {
    repeat {
      ot <- obest+runif(n, min=-2, max=+2) 
      ot <- rank(ot)
      if (!all(ot==obest)) break
    }
    args$y <- y[ot]
    rt     <- do.call(cor, args)    
    if (abs(rt-r)<abs(rbest-r)) {
      rbest <- rt
      obest <- ot
    }
    i <- i+1
  }
  xy <- cbind(x,y[obest])  
  if (args$method=="pearson") {
    xyn <- scale(xy, scale=FALSE)    
    m <- rbind(t(xy), t(xyn), t(xyn^2), xyn[,1]*xyn[,2]) 
    row.names(m) <- c("$x_i$", "$y_i$", "$(x_i-\\bar{x})$", "$(y_i-\\bar{y})$", 
                     "$(x_i-\\bar{x})^2$", "$(y_i-\\bar{y})^2$", "$(x_i-\\bar{x})(y_i-\\bar{y})$")
  }
  if (args$method=="spearman") {
    xys <- xy[order(xy[,1], )]
    xyn <- apply(xys, 2, rank)
    m   <- rbind(t(xys), t(xyn), (xyn[,1]-xyn[,2])^2)
    row.names(m) <- c("$x_i$", "$y_i$", "$rank(x_i)$", "$rank(y_i)$", "$d_i^2$")
  }
  if (args$method=="kendall") {
    xys <- xy[order(xy[,1]),]
    m   <- rbind(t(xys), rowSums(outer(xys[,1], xys[,1], "<") & outer(xys[,2], xys[,2], "<")),
                 rowSums(outer(xys[,1], xys[,1], "<") & outer(xys[,2], xys[,2], ">")))
    row.names(m) <- c("$x_i$", "$y_i$", "$p_i$", "$q_i$")
  } 
  m <- cbind(m, rowSums(m))
  colnames(m) <- c(1:n, "$\\sum$") 
  structure(xy, interim=m)
}

#' @rdname cor_data
#' @export
# dcorr <- function(...){
#  cor_data(...)}
dcorr <- cor_data
