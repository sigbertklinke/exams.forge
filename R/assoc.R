#' @title Association and Correlation Measures
#' @description Compute association and correlation measures for categorical and ordinal data.
#'
#' The following measures are implemented:
#' * **nom.cc**: Corrected contingency coefficient for nominal data.
#' * **nom.cramer**: Cramer's V (or Phi) for nominal data.
#' * **ord.spearman**: Spearman's rank correlation for ordinal data.
#' * **ord.kendall**: Kendall's rank correlation for ordinal data.
#'
#' @rdname assoc
#' @aliases cc_coef
#' @aliases cramer_vf
#' @aliases cramer_coef
#' @aliases kendall_corr
#' @aliases spearman_corr
#' @aliases rs_corr
#'
#' @param tab A contingency table (matrix or table) with absolute frequencies.
#' @param correct Logical, whether to apply a correction (default: \code{FALSE}). Only used for \code{nom.cc}.
#' @param ... Additional parameters passed to correlation functions.
#'
#' @return A numeric value representing the association or correlation measure.
#'
#' @details
#' These functions provide common measures of association:
#' - Nominal data: \code{nom.cc}, \code{nom.cramer}.
#' - Ordinal data: \code{ord.spearman}, \code{ord.kendall}.
#'
#' @export
#' @md
#'
#' @examples
#' # Create a random contingency table
#' tab <- matrix(round(10 * runif(15)), ncol = 5)
#'
#' # Nominal association
#' nom.cc(tab)
#' nom.cc(tab, correct = TRUE)
#' nom.cramer(tab)
#'
#' # Ordinal correlation
#' ord.spearman(tab)
#' ord.kendall(tab)
#'
#' # Using aliases
#' cc_coef(tab)
#' cramer_vf(tab)
#' spearman_corr(tab)
#' kendall_corr(tab)
#' rs_corr(tab)
nom.cc <- function(tab, correct=FALSE) {
  nthroot <- function (x, n) {
    if (!is.numeric(x)) 
      stop("Argument 'x' must be numeric.")
    if (missing(n) || n <= 0 || ceiling(n) != floor(n)) 
      stop("Argument 'n' must be a positive integer.")
    if (any(x[!is.na(x)] < 0) && n%%2 == 0) 
      stop("If argument 'x' is negative, 'n' must be an odd integer.")
    sx <- sign(x)
    return(sx * (sx * x)^(1/n))
  }
  #
  n     <- sum(tab)
  row   <- rowSums(tab)
  col   <- colSums(tab)
  expe  <- (row%o%col)/n 
  chi2  <- sum((tab-expe)^2/expe)
  coeff <- sqrt(chi2/(chi2+n))
  if (correct) {
    nr <- length(row)
    nc <- length(col)
    coeff <- coeff/nthroot((nr-1)/nr*(nc-1)/nc, 4)
  }
  coeff
}

#' @rdname assoc
#' @export
nom.cramer <- function(tab, ...) {
  n     <- sum(tab)
  row   <- rowSums(tab)
  col   <- colSums(tab)
  expe  <- (row%o%col)/n 
  chi2  <- sum((tab-expe)^2/expe)
  k    <- min(length(row), length(col))
  sqrt(chi2/(k*n))
}

#' @rdname assoc
#' @importFrom stats cor
#' @export
ord.spearman <- function(tab, ...) {
  wtab  <- as.data.frame.table(tab)
  index <- rep(1:nrow(wtab), wtab$Freq)
  utab  <- wtab[index,]
  cor(as.numeric(utab[,1]), as.numeric(utab[,2]), ..., method="spearman")
}
 
#' @rdname assoc
#' @importFrom stats cor
#' @export
ord.kendall <- function(tab, ...) {
  wtab  <- as.data.frame.table(tab)
  index <- rep(1:nrow(wtab), wtab$Freq)
  utab  <- wtab[index,]
  cor(as.numeric(utab[,1]), as.numeric(utab[,2]), ..., method="kendall")
} 

#' @rdname assoc
#' @export
# cc_coef <- function(...){
#  nom.cc(...)}
cc_coef <- nom.cc

#' @rdname assoc
#' @export
# cramer_vf <- function(...){
#  nom.cramer(...)}
cramer_vf <- nom.cramer

#' @rdname assoc
#' @export
# cramer_coef <- function(...){
#  nom.cramer(...)}
cramer_coef <- nom.cramer

#' @rdname assoc
#' @export
# kendall_corr <- function(...){
#  ord.kendall(...)}
kendall_corr <- ord.kendall

#' @rdname assoc
#' @export
# spearman_corr <- function(...){
#  ord.spearman(...)}
spearman_corr <- ord.spearman

#' @rdname assoc
#' @export
# rs_corr <- function(...){
#  ord.spearman(...)}
rs_corr <- ord.spearman
