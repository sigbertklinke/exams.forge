#' @rdname lm1_data
#' @title Simple Linear Regression and Data Generation
#' @aliases slr_data
#' @description Creates data suitable for a simple linear regression. In the first step, data is computed using [exams.forge::pearson_data()], 
#' satisfying the conditions \eqn{\sum_{i=1}^{nmax} x_i^2 = n} and  \eqn{\sum_{i=1}^{nmax} x_i = 0} (similar conditions apply to \eqn{y}. 
#' The data are then rescaled with \eqn{x' = center[1]+scale[1]*x} and 
#' \eqn{y' = center[2]+scale[2]*y}. 
#' Finally, a simple linear regression is performed on the transformed data.
#'
#' @param r numeric: desired correlation
#' @param n integer: number to decompose as sum of squares, see [exams.forge::pearson_data()]. 
#' @param nmax integer: maximal number of squares in the sum, see [exams.forge::pearson_data()]. 
#' @param maxt numeric: maximal number of seconds the routine should run, see [exams.forge::pearson_data()]. 
#' @param xsos sos matrix: precomputed matrix, see [exams.forge::pearson_data()]. 
#' @param ysos sos matrix: precomputed matrix, see [exams.forge::pearson_data()]. 
#' @param center numeric(2): center of `x` and `y` data 
#' @param scale numeric(2): standard deviation for `x` and `y` data 
#' @param ... further named parameters given to [stats::lm()]
#'
#' @md
#' @return Returns an extended `lm` object and the additional list elements: 
#' * `inter` contains intermediate results (the last column contains the row sums), and
#' * `xy` the generated \eqn{x}- and \eqn{y}-values. 
#' @importFrom stats lm
#' @export
#'
#' @examples
#' data(sos100, package="exams.forge.data")
#' n   <- sample(5:10, 1)
#' lm1 <- lm1_data(0.6, nmax=n, xsos=sos100)
#' str(lm1)
lm1_data <- function(r, n=100, nmax=6, maxt=30, xsos=NULL, ysos=NULL, center=numeric(0), scale=numeric(0), ...) {
  stopifnot(abs(r)<=1)
  xy <- pearson_data(r=r, nmax=nmax, n=n, xsos=xsos, ysos=ysos)
  if (length(center)==0) center <- c(0,0)
  if (length(center)==1) center <- c(center, center)
  if (length(scale)==0) scale <- c(1,1)
  if (length(scale)==1) scale <- c(scale, scale)
  xy  <- cbind(center[1]+scale[1]*xy[,1], center[2]+scale[2]*xy[,2])
  ret <- lm(xy[,2]~xy[,1])
  xyn <- scale(xy, scale=FALSE)    
  m   <- rbind(t(xy), t(xyn), t(xyn^2), xyn[,1]*xyn[,2], t(xy^2), xy[,1]*xy[,2]) 
  m   <- cbind(m, rowSums(m))
  colnames(m)  <- c(1:nmax, "$\\sum$") 
  row.names(m) <- c("$x_i$", "$y_i$", "$(x_i-\\bar{x})$", "$(y_i-\\bar{y})$", 
                    "$(x_i-\\bar{x})^2$", "$(y_i-\\bar{y})^2$", "$(x_i-\\bar{x})(y_i-\\bar{y})$",
                    "$x_i^2$", "$y_i^2$", "$x_i \\cdot y_i$")
  ret$inter <- m
  ret$xy    <- xy
  ret
}  

#' @rdname lm1_data
#' @export
# slr_data <- function(...){
#  lm1_data(...)}
slr_data <- lm1_data
