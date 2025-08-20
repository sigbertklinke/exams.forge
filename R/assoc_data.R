#' @title Optimize Frequency Table for a Target Association
#' @description  
#' Reorders the entries of a frequency table to approximate a given target association or correlation.
#' 
#' The reordering preserves the marginal frequencies of the table. Note that the target association may 
#' not always be achievable, especially for extreme values (e.g., +1, -1, or values near these limits).
#' 
#' @rdname assoc_data
#' @aliases reorder_association_data
#' @aliases dassoc
#'
#' @param tab table A contingency table of absolute frequencies.
#' @param zero logical Whether zeros are allowed in the resulting table (default: \code{FALSE}).
#' @param FUN function A function that computes the association or correlation from a frequency table (default: [nom.cc]).
#' @param target numeric Desired association or correlation value (default: \code{NA}, which returns the original table).
#' @param tol numeric Maximum allowed deviation between the achieved and target association (default: \code{0.001}).
#' @param maxit integer Maximum number of iterations to reach the target (default: \code{500}).
#' @param ... Additional parameters passed to \code{FUN}.
#'
#' @return A frequency table reordered to approximate the target association. The returned object includes attributes:
#' \describe{
#'   \item{\code{iterations}}{Number of iterations performed.}
#'   \item{\code{target}}{Achieved association or correlation value.}
#' }
#'
#' @details
#' The function attempts to reorder the table entries to reach the target association. If the target is extreme 
#' (e.g., +1, -1, or values near these limits), a solution may not be possible. 
#' If \code{attr(joint, "iterations")} equals \code{maxit}, consider increasing \code{maxit}, 
#' reducing \code{tol}, or choosing a more feasible \code{target} value:
#' \itemize{
#'   \item Nominal measures: \eqn{0 \le target \le 1}
#'   \item Ordinal measures: \eqn{-1 \le target \le +1}
#' }
#'
#' @examples
#' tab <- table_data(3, 2)
#' tab
#' tab2 <- assoc_data(tab, target = 0.5)
#' tab2
#'
#' @export
assoc_data <- function(tab, zero=FALSE, FUN=nom.cc, target=NA, tol=0.001, maxit=500, ...) {
  stopifnot(!is.null(dim(tab)))
  it   <- 0
  fun  <- match.fun(FUN)
  curr <- fun(tab, ...)
  if (!is.na(target)) {
    #browser()
    d    <- abs(curr-target)
    if (!equal(d,0, tol)) {
      n  <- sum(tab)
      while(it<maxit) {
        i    <- sample(nrow(tab), size=2)
        j    <- sample(ncol(tab), size=2)
        tabt <- tab
        tabt[i[1], j[1]] <- tabt[[i[1], j[1]]]+1
        tabt[i[1], j[2]] <- tabt[[i[1], j[2]]]-1
        tabt[i[2], j[1]] <- tabt[[i[2], j[1]]]-1
        tabt[i[2], j[2]] <- tabt[[i[2], j[2]]]+1
        doit <- all(if (zero) (tabt[i,j]>=0) else (tabt[i,j]>0), tabt[i,j]<=n)
        if (doit) {
          curr <- fun(tabt, ...)
          dt   <- abs(curr-target)
          if (dt<d) {
            d    <- dt
            tab  <- tabt
            if (equal(d,0, tol)) break
          }
        }
        it <- it+1
      }
    }
  }
  structure(tab, iterations=it, target=fun(tab, ...))
}
#' @rdname assoc_data
#' @export reorder_association_data
# reorder_association_data <- function(...){assoc_data(...)}
reorder_association_data <- assoc_data

#' @rdname assoc_data
#' @export dassoc
# dassoc <- function(...){assoc_data(...)}
dassoc <- assoc_data
