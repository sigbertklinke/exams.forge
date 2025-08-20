#' @rdname latex_helper
#' @aliases latex_sumprod
#' @aliases latex_sum 
#' @aliases latex_product 
#' @aliases latex_mean
#' @aliases latex_var 
#' @aliases latex_bracket
#' @aliases latex_pmsign
#' @title Supporting Functions for Math LaTeX Output
#' @description \code{lsumprod} creates a latex printout of \eqn{\sum_i x_i y_i} with brackets if \eqn{x_i} or \eqn{y_i} starts with a `-`.
#' 
#' @param x numeric: input values
#' @param mu numeric: population mean (default: `NULL`)
#' @param br,left,right character: which brackets to use. The possibilities are: 
#' * `(` (default) uses `\left(` and `\right(`, 
#' * `[` use `\left[` and `\right]`, 
#' * \code{\{} use `\left\{` and `\right\}`,
#' * `|`  use `\left|` and `\right|`,
#' * `||`  uses `\left\|` and `\right\|`,
#' * `<`, `a` use `\left\langle` and `\right\rangle`,
#' * `c`  use `\left\lceil` and `\right\rceil`, and
#' * `f`  use `\left\lfloor` and `\right\rfloor`. 
#' @param subset logical: indicates which elements have brackets added (default: `NULL` = all elements starting with `-`); missing values are taken as false.
#' @param collapse character: an optional character string to separate the results (default: ', ')
#' @param ... further input values
#' 
#' @return A character.
#' @export
#'
#' @examples
#' lsumprod(-2:2, (1:5)/10)
#' lbr(-2:2)
#' lsum(-2:2)
#' lmean(-2:2)
#' lvec(-2:2)
#' lvec(-2:2, '[')
#' lvec(0:1, '(', ']')
lsumprod <- function(..., br="(") {
  args <- list(...)
  stopifnot(length(args)>1)
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  ret <- lbr(args[[1]])
  for (i in 2:length(args)) ret <- paste(ret, '\\cdot', lbr(args[[i]], br))
  paste0(ret, collapse=" + ")
}

#' @rdname latex_helper 
#' @description \code{lsum} creates a latex printout of \eqn{x} as sum.
#' @export 
lsum <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  paste0(c(x[1], lsgn(x[-1])), collapse="")
}

#' @rdname latex_helper 
#' @description \code{lprod} creates a latex printout of \eqn{x} as product.
#' @export 
lprod <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  paste0(lbr(x), collapse=" \\cdot ")
}

#' @rdname latex_helper 
#' @description \code{lvec} creates a latex printout of \eqn{x} as vector.
#' @export 
lvec <- function(x, left=c("(", "[", "{", "|", "||", "<", "a", "c", "f"), right=NULL, collapse = ', ') {
  bracket <- match.arg(left)
  paste0(the$opening[bracket], paste0(x, collapse=collapse), 
         if (is.null(right)) the$closing[bracket] else right)
}

#' @rdname latex_helper 
#' @description \code{lmean} creates a latex printout as \eqn{\frac{x_1+...+x_n}{n}}.
#' @export 
lmean <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  paste0("\\frac{", lsum(x), "}{", as.character(length(x)), "}")
}

#' @rdname latex_helper 
#' @description \code{lvar} creates a latex printout as \eqn{\frac{(x_1-xbar)^2+...+(x_n-xbar)^2}{n}}.
#' @export 
lvar <- function(x, mu=NULL, br="(") {
  oo  <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  if (is.null(mu)) {
    n  <- length(x)-1
    xm <- lbr(mean(x))
  } else {
    n  <- length(x)
    xm <- lbr(mu)
  }
  ret <- paste0("\\left(", x, "-", xm, "\\right)^2", collapse=" + ") 
  paste0("\\frac{", ret, "}{", n, "}")
}

#' @rdname latex_helper 
#' @description \code{lbr} creates a latex printout of \eqn{x} with brackets if \eqn{x} starts with a `-`.
#' @export 
lbr <- function(x, br=c("(", "[", "{", "|", "||", "<", "a", "c", "f"), subset=NULL) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  bracket <- match.arg(br)
  ret <- trimws(as.character(x))
  if (is.null(subset)) subset <- startsWith(ret, "-") else rep_len(subset, length(x))
  subset <- !is.na(subset) & subset
  paste0(ifelse(subset, the$opening[bracket], ""), ret, ifelse(subset, the$closing[bracket], ""))  
}

#' @rdname latex_helper 
#' @description \code{lsgn} creates a latex printout of \eqn{x} with a plus or minus at the beginning.
#' @export 
lsgn <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  ret <- trimws(as.character(x))
  minus <- startsWith(ret, '-')
  paste0(ifelse(minus, '', '+'), ret)  
}

#' @rdname latex_helper
#' @export
# latex_sumprod <- function(...){
#  lsumprod(...)}
latex_sumprod <- lsumprod

#' @rdname latex_helper
#' @export
# latex_sum <- function(...){
# lsum(...)}
latex_sum <- lsum

#' @rdname latex_helper
#' @export
# latex_product <- function(...){
#  lprod(...)}
latex_product <- lprod

#' @rdname latex_helper
#' @export
# latex_mean <- function(...){
#  lmean(...)}
latex_mean <- lmean

#' @rdname latex_helper
#' @export
# latex_var <- function(...){
#  lvar(...)}
latex_var <- lvar

#' @rdname latex_helper
#' @export
# latex_bracket <- function(...){
#  lbr(...)}
latex_bracket <- lbr

#' @rdname latex_helper
#' @export
# latex_pmsign <- function(...){
#  lsgn(...)}
latex_pmsign <- lsgn


#' @rdname latexdef
#' @title Exam PDF with LaTeX
#' @aliases answercol
#' @aliases add_answercol_def
#' @description If exams is called by `exams2pdf`, 
#' * `latexdef` adds a TeX macro by `\def\name{body}` and
#' * `answercol` adds a `\def\answercol{n}` to modify the number of output columns for multiple-choice answers
#' to the LaTeX file.
#' @param name character: macro name
#' @param body,n character: macro body 
#'
#' @return Nothing
#' @export
#'
#' @examples
#' answercol(2)
latexdef <- function(name, body) {
  stopifnot(all(unlist(strsplit(name, '')) %in% c(letters, LETTERS)))
  if (calledBy("exams2pdf")) writeLines(c(sprintf("\\def\\%s{%s}", name, as.character(body)), "")) 
}

#' @rdname latexdef
#' @export
answercol <- function(n) { 
  stopifnot(n>1)
  latexdef("answercol", n) 
} 

#' @rdname latexdef
#' @export
# add_answercol_def <- function(...){
#  answercol(...)}
add_answercol_def <- answercol
