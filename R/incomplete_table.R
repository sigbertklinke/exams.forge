#' @rdname incomplete_table
#' @aliases cont_table_fill
#' @title Relative Contingency Table Fill
#' @description Fills a relative contingency table with `n` missing values, such that the table entries can be
#' recomputed. In case that no solution can be found, an error is generated.
#'
#' @param tab table: a contingency table 
#' @param n integer: number of missing values
#' @param maxit integer: number of maximal iterations (default: `1000`)
#'
#' @return A contingency table including marginal values and total sum with missing values. 
#' The attribute `fillin` gives the necessary information about the order in which the entries can be calculated, 
#' while the attribute `full` presents the contingency table, including marginal values and total sum.
#' 
#' @export
#'
#' @examples
#' tab <- rbind(c(0.02, 0.04, 0.34), c(0.02, 0.28, 0.3))
#' incomplete_table(tab, 7)
incomplete_table <- function(tab, n, maxit=1000) {
  is_solvable <- function(tabl) {
    # try to fill in each step at least one entry
    step <- matrix(NA, ncol=2, nrow=0)
    repeat {
      rows <- which(rowSums(tabl)==1)    
      cols <- which(colSums(tabl)==1)
      if ((length(rows)==0) && (length(cols)==0)) return(NULL)
      for (i in rows) {
        tabl[i, tabl[i,]==1] <- 0
        step <- rbind(step, c(i, which(tabl[i,]==1)))
      }
      for (i in cols) {
        tabl[tabl[,i]==1, i] <- 0
        step <- rbind(step, c(which(tabl[,i]==1), i))
      }
      if (sum(tabl)==0) return(step)
    }
  }
  #
  stopifnot(n<=nrow(tab)+ncol(tab)+2)
  tab <- tab/sum(tab)
  while(maxit) {
    # is solvable?
    tabl     <- matrix(0, ncol=ncol(tab)+1, nrow=nrow(tab)+1)
    rc       <- sample(length(tabl)-1, n-1)
    tabl[rc] <- 1
    solve    <- is_solvable(tabl)
    if (!is.null(solve)) break  
    maxit <- maxit-1
  }
  tabl[length(tabl)] <- 1
  stopifnot(maxit>0)
  tab <- cbind(tab, rowSums(tab))
  tabna <- tab <- rbind(tab, colSums(tab))
  tabna[tabl==1] <- NA
  structure(tabna, fillin=solve, full=tab)
}

#' @rdname incomplete_table
#' @export
# cont_table_fill <- function(...){
#  incomplete_table(...)}
cont_table_fill <- incomplete_table
