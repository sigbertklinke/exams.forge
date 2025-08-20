#' @rdname distributions
#' @title Distributions
#'
#' @description A data frame with the R function names, LaTeX names, discreteness and package origin of a distribution.
#'
#' @docType data
#'
#' @usage data(distributions)
#'
#' @format A data frame with columns \code{r}, \code{latex}, \code{discret} and \code{package}
#'
#' @keywords datasets
#'
#' @examples
#' data(distributions)
#' distributions
"distributions"

#' @rdname skalenniveau
#' @title Skalenniveau
#'
#' @description A data frame with the variables and level of measurement type. The names are in German.
#'
#' @docType data
#'
#' @usage data(skalenniveau)
#'
#' @format A data frame with columns \code{var}, and \code{type}.
#'
#' @keywords datasets
#'
#' @examples
#' data(skalenniveau)
#' head(skalenniveau)
"skalenniveau"

#' @rdname sos
#' @title Precomputed Sum of Squared Data
#' @aliases sos100 sos200 sos400 sos800 sos1000
#' @description
#' Five data matrices with precomputed results from \code{sumofsquares(n, 10, zerosum=TRUE, maxt=Inf)} for
#' \code{n=100}, \code{n=200}, \code{n=400},\code{n=800}, and \code{n=1000}.
#'
#' @docType data
#'
#' @usage
#' data(sos100)
#' data(sos200)
#' data(sos400)
#' data(sos800)
#' data(sos1000)
#'
#' @format For each line of a matrix it holds \eqn{\sum_{i=1}^k x_i^2=n} and \eqn{\sum_{i=1}^k x_i=0}.
#' It contains all integer solutions up to \code{k<=10}. \code{NA} means that this entry is not used.
#'
#' @keywords datasets
#'
#' @examples
#' data(sos100)
#' head(sos100)
#' rowSums(sos100^2, na.rm=TRUE)
#' rowSums(sos100, na.rm=TRUE)
"sos100"

#' @rdname sos
#' @format NULL
"sos200"

#' @rdname sos
#' @format NULL
"sos400"

#' @rdname sos
#' @format NULL
"sos800"

#' @rdname sos
#' @format NULL
"sos1000"
