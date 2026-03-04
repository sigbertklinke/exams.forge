#' @rdname toHTML
#' @aliases toLatex toHTMLorLatex
#' @title Export Matrices as HTML or LaTeX
#' @description Convert a matrix to a formatted representation in HTML or LaTeX:
#' 
#' * `toHTML`: Returns an HTML table of a matrix. Optionally displays it in a browser
#'   via a temporary file using [utils::browseURL()].
#' * `toLatex`: Returns a LaTeX table representation. Supports a subset of style options.
#' * `toHTMLorLatex`: Chooses HTML or LaTeX output based on the presence of `exams2pdf` 
#'   in the call stack.
#'
#' @param x,object An object of class `html_matrix`.
#' @param browser Logical; if `TRUE`, the HTML output is opened in a browser (default `FALSE`).
#'   Only used by `toHTML`.
#' @param delay Numeric; seconds to wait before deleting temporary HTML files. A value of `0`
#'   keeps the file until the R session ends. Only used by `toHTML`.
#' @param ... Additional arguments passed to [utils::browseURL()] (only relevant for `toHTML`).
#'
#' @return A character string containing the HTML or LaTeX representation of the matrix.
#' @importFrom utils browseURL
#' @importFrom tools toHTML
#' @export
#'
#' @examples
#' library("tools")
#' m  <- matrix(1:12, ncol = 4)
#' hm <- html_matrix(m)
#' # toHTML(hm, browser = TRUE) # opens into browser
#' toHTML(hm)
#' toLatex(hm)
#' toHTMLorLatex(hm)

toHTML.html_matrix <- function(x, browser=FALSE, delay=2, ...)  {
  style <- function(l) {
    use <- setdiff(names(l), c("tooltip", "value", "fmt", "")) 
    if (length(use)==0) return('')
    use2 <- gsub("_", "-", use, fixed=TRUE)
    txt <- ' style="'
    for (k in seq(use2)) {
      txt <- paste0(txt, paste0(use2[k], ':', as.character(l[[use[k]]]), ';'))
    }
    paste0(txt, '"')
  }
  #
  stopifnot("html_matrix" %in% class(x))
  tooltip <- attr(x, "tooltip")
  tabletitle <- if (is.null(tooltip)) '' else sprintf(' title="%s"', tooltip)
  html <- paste0("<p><table", style(attr(x, "table")), tabletitle, ">\n")
  tr    <- attr(x, "tr")
  title <- attr(x, "title")
  rows  <- attr(x, "rownames")
  cols  <- attr(x, "colnames")  
  for (r in 0:nrow(x)) {
    html <- paste0(html, "<tr", style(tr[[r+1]]), ">")
    for (c in 0:ncol(x)) {
      if (r) {
        if (c) {
          html <- paste0(html, "<td", style(x[[r,c]]), ">", sprintf(x[[r,c]]$fmt, x[[r,c]]$value), "</td>")  
        } else {
          html <- paste0(html, "<td", style(rows[[r]]), ">", sprintf(rows[[r]]$fmt, rows[[r]]$value), "</td>")       
        }
      } else {
        if (c) {
          html <- paste0(html, "<td", style(cols[[c]]), ">", sprintf(cols[[c]]$fmt, cols[[c]]$value), "</td>")              
        } else {
          html <- paste0(html, "<td", style(title), ">", sprintf(title$fmt, title$value), "</td>")  
        }
      }
    }
    html <- paste0(html, "</tr>\n")
  }
  html <- paste0(html, "</table></p>") 
  if (browser) {
    file <- tempfile(fileext=".html")
    writeLines(html, file)
    browseURL(file, ...)
    cleanFile(file, delay)
  }
  html
}

#' @rdname toHTML
#' @export
toLatex.html_matrix <- function(object, ...) {
  style <- function(l, cont, colenv) {
    use <- setdiff(names(l), c("tooltip", "value", "fmt", "")) 
    ret <- cont
    if (length(use)==0) return(ret)
    mc <- ''
    if (!is.null(l$background_color)) {
      col <- l$background_color
      if (startsWith(col, '#')) {
        col <- toupper(substring(col, 2))
        hmc <- colenv$hmc
        pos <- which(hmc$html==col)
        if (length(pos)==0) {
          pos <- min(which(hmc$html==''))
          hmc[pos, 'html'] <- col
          colenv$hmc <- hmc
        }
        col <- hmc[pos, 'name']
      } 
      mc <- paste0('>{\\columncolor{', col, '}}')
    }   
    if(!is.null(l$text_align)) mc <- paste0(mc, substr(l$text_align, 1, 1))
    if (!is.null(l$font_weight)) ret <- paste0("\\textbf{", ret, '}')
    paste0("\\multicolumn{1}{", mc, '}{', ret, '}')    
  }
  #
  x <- object
  stopifnot("html_matrix" %in% class(x))
#  tooltip <- attr(x, "tooltip")
#  tabletitle <- if (is.null(tooltip)) '' else sprintf(' title="%s"', tooltip)
  colors <- new.env()
  index  <- 0:675
  colors$hmc <- data.frame(name=paste0("htmlmatrix", LETTERS[1+(index%/%26)], LETTERS[1+(index%%26)]),
                           html=rep('', length(index)))
  latex <- c("\\begin{table}[h]", '\\centering', paste0("\\begin{tabular}{", paste0(rep('c', ncol(x)+1), collapse=""), '}'))
#  tr    <- attr(x, "tr")
  title <- attr(x, "title")
  rows  <- attr(x, "rownames")
  cols  <- attr(x, "colnames")  
  for (r in 0:nrow(x)) {
#    latex <- paste0(latex, "<tr", style(tr[[r+1]]), ">")
    lrow <- ''
    for (c in 0:ncol(x)) {
      if (r) {
        if (c) {
          lrow <- paste0(lrow, ' & ', style(x[[r,c]], sprintf(x[[r,c]]$fmt, x[[r,c]]$value), colors))
        } else {
          lrow <- paste0(lrow, style(rows[[r]], sprintf(rows[[r]]$fmt, rows[[r]]$value), colors))       
        }
      } else {
        if (c) {
          lrow <- paste0(lrow, ' & ', style(cols[[c]], sprintf(cols[[c]]$fmt, cols[[c]]$value), colors))   
        } else {
          lrow <- paste0(lrow, style(title, sprintf(title$fmt, title$value), colors))
        }
      }
    }
    latex <- c(latex, paste0(lrow, "\\\\"))
  }
  latex <- c(latex, "\\end{tabular}", "\\end{table}") 
  index <- which(colors$hmc[,'html']!='')
  for (i in seq_along(index)) {
    latex <- c(sprintf("\\definecolor{%s}{HTML}{%s}", colors$hmc[i,'name'], colors$hmc[i,'html']), latex)
  }
  paste0(latex, collapse="\n") 
}

#' @rdname toHTML
#' @export
toHTMLorLatex <- function(x, ...) {
  stopifnot("html_matrix" %in% class(x))
  if (calledBy('exams2pdf')) toLatex(x) else toHTML(x) 
}
