library("knitr")
library("latex2exp")

distributions <- new.env()

craft <- function(name, ..., .update=TRUE) {
  #browser()
  dmiss <- is.null(distributions[[name]])
  if (.update) {
    if (dmiss) stop(sprintf("Distribution '%s' not found", name))
    distributions[[name]]$a
  } else {
    if (!dmiss) warning(sprintf("Overwriting distribution '%s'", name))
    dfun <- list(a=NULL)
    fun  <- paste0(c('d', 'p', 'q', 'r'), name)
    for (letter in c('d', 'p', 'q', 'r')) {
      f <- try(match.fun(paste0(letter, name)), silent = TRUE)
      if (!inherits(f, 'try-error')) dfun[[letter]] <- f 
    }  
    if (is.null(dfun$d)) stop(sprintf("Function `d%s` not found", name))
    distributions[[name]] <- dfun
  }
  args <- list(...)
  if (length(args)) {
    for (aname in names(args)) {
      if (tolower(aname)!=aname) stop(sprintf("All properties must be lowerspace: %s", aname)) 
      distributions[[name]]$a[[aname]] <- args[[aname]]
    }
  }
  pnames <- setdiff(names(distributions[[name]]$a), c("latex", "print", "plotmath", "expectation", "variance"))
  for (pname in pnames) {
    fname <- paste0(toupper(substr(pname, 1, 1)), substring(pname, 2)) 
    if(!exists(fname, mode="function")) {
      fun <- eval(parse(text=sprintf("function(d) { prop(d, '%s') } ", pname)))
      assignInMyNamespace(fname, fun)
    }
  }
}

# old: distribution.default <- function(name, ..., discrete=NA) 
distribution <- function(name, ...) {
  # browser()
  if (missing(name)) {
    names <- names(distributions)
    ret   <- list()
    for (i in 1:length(names)) {
      cat(sprintf("%s:\n", names[i]))
      dan <- sort(names(distributions[[names[i]]]$a))
      ret[[names[i]]] <- dan
      fun <- c("d"="pmdf", "p"="cdf", "q"="quantile", "r"="random")
      fun <- fun[names(fun) %in% names(distributions[[names[i]]])] 
      dan <- paste0(paste0(dan, collapse=", "), paste0(" (", paste0(fun, collapse=", "), ")"))
      dan <- paste0(strwrap(dan, prefix="  "), "\n") 
      cat(dan, "\n")
    }
    return(invisible(dan))
  }
  stopifnot(name %in% names(distributions))
  d    <- distributions[[name]]
  args <- list(...)
  da <- d$a
  for (name in setdiff(names(da), names(args))) args[[name]] <- da[[name]]
  if (length(da)) {
    da <- lapply(args, eval, envir=args)
    for (i in 1:length(da)) {
      if (is.character(da[[i]])) da[[i]] <- knit(text=da[[i]], envir=list2env(da))
    }
    #
    if(!is.null(da$latex) && is.null(da$print)) da$print <- gsub("[\\\\\\\\]", "", da$latex)
    if(!is.null(da$latex) && is.null(da$plotmath)) da$plotmath <- TeX(da$latex)
    d$a <- da
  }
  #  }
  if (!is.null(d$a$excess)) d$a$kurtosis <- d$a$excess+3
  d$call <- match.call()
  structure(d, class=unique('distribution', class(d)))
}

# new
print.distribution <- function(x, ...) { 
  txt <- x$a$print
  if (is.null(txt)) txt <- deparse(x$call)
  txt    
}

# new
summary.distribution <- function(object, ...) {
  cat(deparse(object$call[[2]]), "\n")
  str(object$a, give.attr = FALSE)
}

# old: quantile.distribution <- function(x, probs=seq(0, 1, 0.25), ...) 
qdist <- quantile.distribution <- function(d, p=(0:4)/4) {
  fun <- d$q
  if (is.null(fun)) stop(sprintf("No `quantile` function for distribution `%s` available", name))
  do.call(fun, list(p=p), envir=list2env(d$a))
}

# new
rdist <- random <- function(d, n) {
  fun <- d$r
  if (is.null(fun)) stop(sprintf("No `random` function for distribution `%s` available", name))
  do.call(fun, list(n=n), envir=list2env(d$a))
}

# old: cdf <- function(d, q, ...) 
pdist <- cdf <- function(d, q) {
  fun <- d$p
  if (is.null(fun)) stop(sprintf("No cumulative distribution function for distribution `%s` available", name))
  do.call(fun, list(q=q), envir=list2env(d$a))
}

# old: pmdf <- function(d, x, ...) 
ddist <- pmdf <- function(d, x) {
  fun <- d$d
  if (is.null(fun)) stop(sprintf("No probability mass/density function for distribution `%s` available", name))
  do.call(fun, list(x=x), envir=list2env(d$a))  
}

# old: toLatex.distribution <- function(object, name=NULL, param=NULL, digits=4, ...) 
toLatex.distribution <- function(object, ...) {
  if (is.null(object$a$latex)) stop("Property 'latex' not found")
  object$a$latex
} 

# new
toString.distribution <- function(object, ...) {
  if (is.null(object$a$print)) stop("Property 'print' not found")
  object$a$print
} 

# new
toMath <- function(object, ...) {
  stopifnot(inherits(object, 'distribution'))
  if (is.null(object$a$plotmath)) stop("Property 'plotmath' not found")
  object$a$plotmath
} 

prop <- function(d, ..., simplify=TRUE) {
  stopifnot(inherits(d, 'distribution'))
  #browser()
  args <- list(...)
  if (length(args)>0) {
    name <- as.character(unlist(args))
    name <- pmatch(name, names(d$a))
    ret  <- d$a[name]
    if (simplify) ret <- simplify2array(ret)
  } else {
    ret <- names(d$a) 
  }
  ret
}

E        <- function(d) { prop(d, "expectation") }
Var      <- function(d) { prop(d, "variance") }

ifint <- function(x, vec, res) { res[1+findInterval(x, vec)] }

# old: is.distribution <- function(object, name=NULL)
# old: prob <- function(d, min=-Inf, max=+Inf, tol = 1e-6) 
# old: prob1 <- function(d, x, tol=1e-6) 

# new:
craft("norm", mean=0, sd=1, package="stats",
      discrete=FALSE, expectation=expression(mean), mode=expression(mean), 
      variance=expression(sd^2), mad=expression(sd*qnorm(0.75)),
      skewness=0, excess=0,
      entropy=expression((1+log(2*pi*sd^2))/2),
      latex="N(\\mu=`r mean`, \\sigma^2=`r sd^2`)",
      .update=FALSE)

craft("t", df=1, package="stats",
      discrete=FALSE, expectation=expression(if(df>1) 0 else NA), median = 0, mode=0,
      variance=expression(ifint(round(df), c(1.5, 4.5), c(NA, Inf, df/(df-2)))), 
      skewness=expression(ifint(round(df), 2.5, c(NA, 0))), 
      excess=expression(ifint(round(df), c(1.5, 4.5), c(NA, Inf, 6/(df-4)))), 
      entropy=expression((df+1)/2*(digamma((df+1)/2)-digamma(df/2))+log(sqrt(df)*beta(df/2, 1/2))),
      latex="t_{`r df`}",
      .update=FALSE)

# new:
dcategorical <- function(x)  {
  index <- which(outcome==x)
  if (length(index)==1) prob[index] else 0
}

# new:
rcategorical <- function(n)  {
  sample(outcome, n, replace=TRUE)
}

# new:
craft("categorical", outcome=1:6, prob=rep(1/6, 6), package="exams.forge",
      discrete=TRUE, latex="Cat(`r paste0(fcvt(prob), collapse=',')`)",
      mode=expression({
        index <- which(prob==max(prob))
        if (length(index)==1) outcome[index] else NA
      },
      .update=FALSE)
)

dn <- distribution("norm", mean=1, sd=0.5)
print(dn)
summary(dn)
E(dn)
Var(dn)
pmdf(dn, 1:3)
cdf(dn, 1:3)
quantile(dn)
random(dn, 10)
toLatex(dn)
toString(dn)
toMath(dn)

dn <- distribution("t", df=3)
print(dn)
summary(dn)

du <- distribution("categorical")
str(du)
