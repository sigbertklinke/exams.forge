#' @rdname cor_data
#' @aliases dcorr
#' @title Correlation and Data Generation
#' @description Generates a data set based on `x` and `y` for a given target correlation `r` according to [stats::cor()].
#' The algorithm modifies the order of the `y`'s, therefore is guaranteed that the (marginal) distribution of `x`
#' and `y` will not be modified. Please note that it is not guaranteed that the final correlation will be the desired correlation; 
#' the algorithm interactively modifies the order. If you are unsatisfied with the result, it might help to increase `maxit`.
#'
#' @param x numeric: given `x` values
#' @param y numeric: given `y` values
#' @param r numeric: desired correlation
#' @param method character: indicates which correlation coefficient is to be computed (default: `"pearson")
#' @param maxit numeric: maximal number of iterations (default: \code{1000})
#' @param ...  further parameters given to [stats::cor()] 
#'
#' @md
#' @return A matrix with two columns and an attribute `interim` for intermediate values as matrix. 
#' The rows of the matrix contain:
#' * if `method=="pearson"`: \eqn{x_i}, \eqn{y_i},  \eqn{x_i-bar{x}}, \eqn{y_i-\bar{y}}, 
#' \eqn{(x_i-bar{x})^2}, \eqn{(y_i-\bar{y})^2}, and \eqn{(x_i-bar{x})((y_i-\bar{y})}.
#' * if `method=="kendall"`:
#'   - \eqn{x_i}: The original x values.
#'   - \eqn{y_i}: The original y values.
#'   - \eqn{p_i}: The number of concordant pairs.
#'   - \eqn{q_i}: The number of discordant pairs.
#' * if `method=="spearman"`: \eqn{x_i}, \eqn{y_i}, \eqn{p_i} (concordant pairs), and \eqn{q_i} (disconcordant pairs).
#' In a final step a vector with the row `sums` is appended as further column.
#' @export
#'
#' @examples
#' x <- runif(6)
#' y <- runif(6)
#' xy <- cor_data(x, y, r=0.6)
#' cbind(x, y, xy)
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
