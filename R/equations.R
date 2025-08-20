#' @rdname equations
#' @title Equations and Variables
#' @description **`equations`** defines a set of equations using the formula interface including a LaTeX representation of the formulae.
#'
#' @param ... For `equations`, an even number of parameters: formula, LaTeX representation, formula, LaTeX representation, etc.\cr \cr  
#' For `variables`, parameters to set one or more variables.
#' 
#' @return (for `equations`) An `equations` object.
#' @export
#'
#' @examples
#' # The equations describe the formulae for an confidence interval of the mean
#' e <- equations(o~x+c*s/sqrt(n), "v_o=\\bar{x}+c\\cdot\\frac{s^2}{n}", 
#'                u~x-c*s/sqrt(n), "v_u=\\bar{x}-c\\cdot\\frac{s^2}{n}", 
#'                e~c*s/sqrt(n),   "e  =c\\cdot\\frac{s^2}{\\sqrt{n}}",
#'                l~2*e,           "l  =2\\cdot e"                   
#'                )
#' print(e)
equations <- function (...) {
  toExpr <- function(formula) {
    lhs <- deparse(formula(formula)[[2]])
    rhs <- deparse(formula(formula)[[3]])
    parse(text=paste0(rhs, "-", lhs)) 
  }
  #
  args <- list(...)
  eqs  <- list()
  j    <- 0
  for (i in 1:length(args)) {
    if (inherits(args[[i]], "formula")) {
      j <- j+1
      eqs[[j]] <- list(expr=toExpr(args[[i]]))
    } else if (inherits(args[[i]], "character")) {
      if (j>0) eqs[[j]]$text <- args[[i]] 
    }
  }
  an <- unique(unlist(lapply(eqs, function(e) { all.vars(e$expr) })))
  for (j in 1:length(an)) eqs[[an[j]]] <- list(value=NA, interval=dbl(5), text=an[j]) 
  structure(eqs, class="equations")
}

#' print.equations
#'
#' Prints an `equations` object with equations and variables. Internally, a data frame is generated, created and printed.
#'
#' @inheritParams base::print
#'
#' @return The data frame invisibly generated.
#' @export
#'
#' @examples
#' # The equations describe the formulae for an confidence interval of the mean
#' e <- equations(o~x+c*s/sqrt(n), "v_o=\\bar{x}+c\\cdot\\frac{s^2}{n}", 
#'                u~x-c*s/sqrt(n), "v_u=\\bar{x}-c\\cdot\\frac{s^2}{n}", 
#'                e~c*s/sqrt(n),   "e  =c\\cdot\\frac{s^2}{\\sqrt{n}}",
#'                l~2*e,           "l  =2\\cdot e"                   
#'                )
#' print(e)
print.equations <- function(x, ...) {
  nob <- names(x)
  if (is.null(nob)) nob <- rep('', length(x))
  df <- data.frame(type     = ifelse(nob=='', "Equation", nob),
                   value    = sapply(x, function(e) { as.character(if (is.null(e$expr)) e$value else e$expr) }),
                   text     = sapply(x, function(e) { e$text  }),
                   interval = sapply(x, function(e) { 
                   if (is.null(e$interval)) NA else sprintf("[%s , %s]", gsub("\\.0+$", "", e$interval[1]), 
                                                                        gsub("\\.0+$", "", e$interval[2])) }) 
             )
  print(df)
}

#' @rdname equations
#' @title Variables 
#' @description **`variables`** sets the variable values, the LaTeX representation and the solution interval. The first argument must be
#' the `equations` object. A named parameter starts the setting for a specific variable, e.g. `..., s=1, pos(5), "s^2",...`
#' sets for the variable `s` first its numerical value, second the solution interval and finally the LaTeX representation.
#'
#' @param ... For `equations`, an even number of parameters: formula, LaTeX representation, formula, LaTeX representation, etc.\cr \cr 
#' For `variables`, parameters to set one or more variables.
#'
#' @return (for `variables`) The modified `equations` object.
#' @export
#'
#' @examples
#' e <- variables(e, 
#'                x=0,    "\\bar{x}",
#'                c=2.58, dbl(2), 
#'                s=1,    pos(5), "s^2",
#'                n=25,   pos(5),
#'                l=pos(5), 
#'                e=pos(5),
#'                u="v_u", o="v_o")
#' print(e)
variables <- function(...) {
  args   <- list(...)
  stopifnot(inherits(args[[1]], "equations"))
  eqs    <- args[[1]]
  nargs  <- names(args)
  nnargs <- nchar(nargs)
  if (is.null(args)) nargs <- rep('', length(args))
  v <- ''
  for (i in 2:length(args)) {
    if (nnargs[i]) { 
      if (is.null(eqs[[nargs[i]]])) stop(sprintf("'%s' not used in equations", nargs[i]))
      v <- nargs[i]
    }
    if (is.numeric(args[[i]])) {
      if (length(args[[i]])==1) eqs[[v]]$value <- args[[i]]
      if (length(args[[i]])>1) eqs[[v]]$interval <- range(args[[i]])
    } else {
      eqs[[v]]$text <- as.character(args[[i]])
    }
  }
  eqs
}

#' @rdname IntervalRanges
#' @aliases dbl
#' @aliases idbl
#' @aliases pos
#' @aliases ipos
#' @aliases neg
#' @aliases ineg
#' @title Interval Ranges
#' @description Generates intervals based on powers of ten.
#'
#' @param pow numeric: power of ten to create intervals
#'
#' @return A `numeric` object.
#' @export
#'
#' @examples
#' dbl(2)
#' dbl(3)
#' pos(3)
#' neg(3)
pos <- function(pow) { c(0, 10^pow) }

#' @rdname IntervalRanges
#' @export
neg <- function(pow) { c(-10^pow, 0) }

#' @rdname IntervalRanges
#' @export
dbl <- function(pow) { c(-10^pow, 10^pow) }

#' @rdname num_solve
#' @title Target Variable Value
#' @aliases sequation
#' @description  Given a set of equations and some variables, `num_solve` tries to compute the value of the `target` variable.
#' The equations \eqn{y=f(x)} are transformed to \eqn{f(x)-y} and the functions try to compute the roots of the equations
#' using [[stats::uniroot()]]. 
#' If the computation fails, then, `numeric(0)` is returned, otherwise the "original" value. If `target==''` then all computed values 
#' and steps are returned. The attribute `compute` contains a data frame. 
#'
#' @param target character: name of the variable value to compute
#' @param eqs an `equations` object
#' @param tol numeric: maximal tolerance for [stats::uniroot()]
#' @param ... further arguments 
#'
#' @return (for `num_solve`) Returns `numeric(0)`, `numeric(1)`, or a list of all (computed) values.
#' @importFrom stats uniroot
#' @export 
#'

num_solve <- function(target, eqs, tol=1e-6) {
  stopifnot(inherits(eqs, "equations"))
  stopifnot("'target' not found in variable list" = target %in% names(eqs))
  funs     <- lapply(eqs, function(e) { e$expr })
  funs     <- funs[!sapply(funs, is.null)]
  interval <- lapply(eqs, function(e) { e$interval })
  interval <- interval[!sapply(interval, is.null)]
  val      <- lapply(eqs, function(e) { e$value })
  compute  <- which(sapply(val, function(e) { !(is.null(e) || is.na(e))  }))
  #browser()
  val      <- val[!sapply(val, is.null)]
  nvfun    <- rep(0, length(funs))
  lc <- i <- 0
  while (i-lc<=length(funs)) {
    fi <- 1+(i%%length(funs))
    v  <- all.vars(funs[[fi]])
    nv <- sapply(val, is.na)[v]
    if (sum(nv)==0) {
      expr <- do.call(substitute, list(expr=funs[[fi]][[1]], env=val))
      res  <- eval(expr)
      if(abs(res)>tol) {
        msg <- c("Tolerance check failed\n", 
                 paste(res, deparse(funs[[fi]]), "\n"),
                 paste(deparse1(funs[[fi]]), "\n")
                )
        #cat(res, deparse1(funs[[fi]]), "\n")
        #cat(unlist(val), "\n")
        stop(msg)
      }
    }
    if (sum(nv)==1) {
      vna         <- names(nv)[nv] 
      vals        <- val
      vals[[vna]] <- quote(.x)
      f           <- function(.x) { }
      body(f)     <- do.call(substitute, list(expr=funs[[fi]][[1]], env=vals))
      zero        <- uniroot(f, interval[[vna]])
      if (is.numeric(zero$f.root)) {
        lc          <- i
        val[[vna]]  <- zero$root 
        compute <- c(compute, structure(fi, names=vna))
        #cat(sprintf("%s=%.4f: ", vna, zero$root), deparse(funs[[fi]]), "\n")
      }
    }                
    i <- i+1    
  }
  result <- val
  if (target!='') {
    if (target %in% names(compute)) {
      ncomp  <- names(compute)
      neqs   <- names(eqs)
      vars   <- target      
      comp   <- numeric()
      while(length(vars)) {
        pos   <- which(vars[1]==ncomp)
        index <- compute[pos]
        comp  <- c(comp, structure(compute[pos], names=vars[1]))
        vars  <- vars[-1]
        if (neqs[index]=='') {
          vars <- setdiff(unique(c(vars, all.vars(eqs[[index]]$expr))), names(comp))
        }
      }
      compute <- rev(comp)
      result <- val[[target]]
    } else {
      result <- numeric(0)
    }
  } 
  latex <- NULL
  comp  <- NULL
  if (length(compute)) {
    #browser()
    neqs   <- names(eqs)
    ncomp  <- names(compute)
    pos    <- match(ncomp, names(val))
    comp   <- list(variable=ncomp, value=as.numeric(unlist(val[pos])), formula=rep(NA_character_, length(compute)))
    for (i in 1:length(compute)) {
      if (neqs[compute[i]]=='') { # formula
        latex           <- c(latex, paste(eqs[[compute[i]]]$text, " & \rightarrow ", eqs[[ncomp[i]]]$text, " = ", exams.forge::fcvt(val[[ncomp[i]]]))) 
        comp$formula[i] <- as.character(eqs[[compute[i]]]$expr)     
      } else { # given value
        latex   <- c(latex, paste("&", eqs[[ncomp[i]]]$text, " = ", fcvt(val[[ncomp[i]]]))) 
      }
    }
    latex <- c("\\begin{align*}", paste0(latex, "\\\\"), "\\end{align*}")
  }
  structure(result, latex=latex, compute=as.data.frame(comp), class="equation_solve")
}

#' @rdname num_solve
#' @title toLatex.equation_solve
#' @description `toLatex.equation_solve` returns a LaTeX representation of the solution way found by `num_solve()`.
#'
#' @inheritParams utils::toLatex
#'
#' @return (For `toLatex.equation_solve`) A character vector.
#' @export
#'
#' @examples
#' # The equations describe the formulae for an confidence interval of the mean
#' e <- equations(o~x+c*s/sqrt(n), "v_o=\\bar{x}+c\\cdot\\frac{s^2}{n}", 
#'                u~x-c*s/sqrt(n), "v_u=\\bar{x}-c\\cdot\\frac{s^2}{n}", 
#'                e~c*s/sqrt(n),   "e  =c\\cdot\\frac{s^2}{\\sqrt{n}}",
#'                l~2*e,           "l  =2\\cdot e"                   
#'                )
#' e <- variables(e, 
#'                x=0,    "\\bar{x}",
#'                c=2.58, dbl(2), 
#'                s=1,    pos(5), "s^2",
#'                n=25,   pos(5),
#'                l=pos(5), 
#'                e=pos(5),
#'                u="v_u", o="v_o")
#' print(e)
#' # Find the confidence interval length
#' ns <- num_solve('l', e)
#' # Compute everything that is possible
#'  ns <- num_solve('', e)
#' toLatex(ns)
#' 
 
toLatex.equation_solve <- function (object, ...) {
  attr(object, "latex")
}

#' @rdname IntervalRanges
#' @export
# idbl <- function(...){
#  dbl(...)}
idbl <- dbl

#' @rdname IntervalRanges
#' @export
# ipos <- function(...){
#  pos(...)}
ipos <- pos

#' @rdname IntervalRanges
#' @export
# ineg <- function(...){
#  neg(...)}
ineg <- neg

#' @rdname num_solve
#' @export
# sequation <- function(...){
#  num_solve(...)}
sequation <- num_solve