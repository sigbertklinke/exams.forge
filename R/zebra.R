#' Apply Zebra Striping to an HTML Matrix
#'
#' This function applies alternating background colors (zebra striping) to an
#' \code{html_matrix} object, either by rows or by columns.
#'
#' @param x An \code{html_matrix} object to which the zebra striping will be applied.
#' @param col A character vector of colors to use for striping. Defaults to
#'   \code{c("#FFFFFF", "#CCCCCC")}.
#' @param byrow Logical; if \code{TRUE}, colors are applied to rows, otherwise
#'   to columns. Default is \code{TRUE}.
#'
#' @return An \code{html_matrix} object with updated background colors.
#'
#' @examples
#' library("magrittr")
#' m  <- matrix(1:12, ncol = 4)
#' hm <- html_matrix(m) %>% zebra()
#' html <- toHTML(hm, browser = TRUE)
#'
#' @export
zebra <- function(x, col=c("#FFFFFF", "#CCCCCC"), byrow=TRUE) {
  stopifnot("html_matrix" %in% class(x))
  if (byrow) {
    col <- rep(col, length.out=nrow(x))    
    for (i in 1:nrow(x)) {
      for (j in 1:ncol(x)) {
        x[[i,j]]$background_color <- col[i] 
      }
    }
  } else {
    col <- rep(col, length.out=ncol(x))    
    for (i in 1:nrow(x)) {
      for (j in 1:ncol(x)) {
        x[[i, j]]$background_color <- col[j] 
      }
    }  
  }
  x
}
