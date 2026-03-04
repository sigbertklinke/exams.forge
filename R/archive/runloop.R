#' @rdname runloop
#' @aliases hyperloop hloop
#' @title Runloop: Iteratively Evaluate a Function
#' @description
#' `runloop()` (also available as `hyperloop()` and `hloop()`) repeatedly
#' calls a function with arguments generated from either all combinations
#' of the values provided in `...` or by repeating a single argument set
#' up to `maxit.` times. Results are collected into a list or simplified array.
#'
#' @param FUN A function to be applied.
#' @param maxit. Integer scalar (>0). Maximum number of iterations when
#'   only a single row of argument combinations exists. This argument is
#'   **internal** to `runloop` and is **not passed** to `FUN`.
#' @param simplify. Logical. If `FALSE` (default), the result is not
#'   simplified. If `TRUE`, results are simplified using
#'   [simplify2array()]. This argument is **internal** and **not passed** to `FUN`.
#' @param ... Arguments to be varied for `FUN`. Each argument may be a
#'   scalar, vector, or list. Arguments of length 0 are not allowed.
#' @param fail. Specifies how errors in `FUN` calls are handled. Can be
#'   one of `"drop"`, `"args"`, `"quiet"`, `"stop"`, `"warn"`, or a
#'   custom function. Default is `"drop"`.
#'
#' @return A list of results. If multiple combinations exist, the list
#'   has one element per combination. If only a single combination is
#'   repeated via `maxit.`, the list has length `maxit.`. If
#'   `simplify.` is `TRUE`, a simplified array or vector may be returned.
#'
#' @details
#' * Each element of `...` is coerced into a list if it is not already.
#' * `expand.grid()` is used to enumerate all index combinations.
#' * `NULL` values inside arguments are skipped.
#' * If only a single combination exists, it is repeated `maxit.` times.
#' * Errors in `FUN` calls are handled according to `fail.`.
#'
#' @examples
#' # Define a simple function
#' f <- function(a, b = 1) a + b
#'
#' # 1) Sweep over multiple arguments (parameter combinations) without simplifying
#' runloop(f, a = 1:3, b = 10)
#'
#' # 2) Same parameter sweep, but simplify the results
#' runloop(f, a = 1:3, b = 10, simplify. = TRUE)
#'
#' # 3) Repeat a single argument combination multiple times without simplifying
#' runloop(f, maxit. = 3, a = 5)
#'
#' # 4) Repeat a single argument combination multiple times and simplify
#' runloop(f, maxit. = 3, a = 5, simplify. = TRUE)
#'
#' # 5) Mixed: one argument varied, one fixed, without simplifying
#' runloop(f, a = 1:2, b = 100)
#'
#' @export
runloop <- function (FUN, maxit.=1000, ..., simplify.=FALSE, 
                     fail.=c("drop", "args",  "quiet", "stop", "warn")) {
  handle_fail <- function(e, args, fail.mode) {
    if (is.character(fail.mode)) {
      switch(fail.mode,
             stop  = stop(e),
             warn  = { 
               warning("runloop(): ", conditionMessage(e))
               structure(list(condition = e), class = "try-error")
             },
             quiet = structure(list(condition = e), class = "try-error"),
             drop  = structure(list(), class = "runloop_drop"),
             args  = structure(list(condition = e, args = args), class = "try-error")
      )
    } else if (is.function(fail.mode)) {
      res <- fail.mode(e, args)
      if (!inherits(res, "try-error")) res <- structure(res, class = c("try-error", class(res)))
      res
    } else stop("Invalid `fail.` argument")
  }
  #
  stopifnot(maxit.>0)
  fail.mode <- if (is.character(fail.)) match.arg(fail.) else fail.
  args  <- list(...)
  nargs <- getNames(args, TRUE)
  g     <- data.frame()
  largs <- length(args)
  if (largs) {
    for (i in 1:largs) {
      if (!is.list(args[[i]])) args[[i]] <- list(args[[i]])
    }
    if (any(lengths(args)==0)) stop("An argument in `...` has length zero")
    g <- do.call("expand.grid", lapply(args, function(sub) seq_along(sub)))
  } 
  if (nrow(g)==1) g <- g[rep(1, maxit.),,drop=FALSE]
  if (nrow(g)==0) g <- data.frame(matrix(ncol = 0, nrow = maxit.))
  ret <- vector("list", nrow(g))
  for (i in 1:nrow(g)) {
    fargs <-  setNames(vector("list", largs), nargs)
    for (j in seq_along(fargs)) {
      valij <- args[[j]][[g[i,j]]]
      if (!is.null(valij)) fargs[[j]] <- valij
    }
    res <- tryCatch(
      do.call(FUN, fargs),
      error = function(e) handle_fail(e, fargs, fail.mode)
    )
    if (inherits(res, "runloop_drop")) next  # skip failed run
    ret[[i]] <- res
  }
  if (!isFALSE(simplify.)) ret <- simplify2array(ret)
  ret
}

#' @rdname hyperloop
#' @export 
# hloop <- function(...){hyperloop (...)}
hyperloop <- runloop

#' @rdname hyperloop
#' @export 
# hloop <- function(...){hyperloop (...)}
hloop <- runloop