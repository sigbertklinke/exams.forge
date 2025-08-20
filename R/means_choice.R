#' @rdname means_choice
#' @title Means
#' @description Computes the means of \code{x}. The list returned has an attribute \code{"mindiff"} which contains 
#' the smallest distance between two mean values before rounding.
#' If \code{winsor} and/or \code{trim} is set to \code{NA} then the trimmed and/or winsorized means are not computed. 
#' Currently implemented are:
#' \describe{
#' \item{\code{mean}}{arithmetic mean}
#' \item{\code{median}}{median}
#' \item{\code{harmonic}}{harmonic mean}
#' \item{\code{geometric}}{geometric mean}
#' \item{\code{mode}}{(first) mode}
#' \item{\code{trim}}{trimmed mean}
#' \item{\code{winsor}}{winsorized mean}
#' }
#'
#' @param x numeric: data values
#' @param digits numeric:	integer indicating the number of decimal points for rounding (negative values are allowed)
#' @param na.rm logical: should \code{NA}s be removed before?
#' @param trim numeric: the fraction (0 to 0.5) of observations to be trimmed from each end of \code{x}
#' @param winsor numeric: the fraction (0 to 0.5) of observations to be moved from each end of \code{x}
#'
#' @importFrom stats median na.omit
#' @return A list with mean values.
#' @export
#'
#' @examples
#' x <- c(runif(9), 3)
#' means_choice(x, 2)
means_choice <- function (x, digits, na.rm=TRUE, trim=0.2, winsor=0.2) {
  winsor.mean <- function (x, trim = 0.2, na.rm = TRUE) { 
    if ((trim < 0) | (trim > 0.5)) stop("trimming must be reasonable")
    if (length(dim(x))) {
      ans <- apply(x, 2, winsor.mean, trim = trim, na.rm = na.rm)
    } else {
      if (trim < 0.5) {
        qtrim <- quantile(x, c(trim, 0.5, 1 - trim), na.rm = na.rm)
        xbot  <- qtrim[1]
        xtop  <- qtrim[3]
        x[x < xbot] <- xbot
        x[x > xtop] <- xtop
        ans <- mean(x, na.rm = na.rm)  
      } else {
        ans <- median(x, na.rm = TRUE)
      }
    }
    return(ans)
  }
  #
  harmonic.mean <- function (x, na.rm = TRUE, zero = TRUE) {
    if (!zero) x[x == 0] <- NA
    if (length(dim(x))) {
      1/(apply(1/x, 2, mean, na.rm = na.rm))
    } else {
      1/mean(1/x, na.rm = na.rm)
    }
  }
  #
  if (missing(digits)) stop("parameter 'digits' is required")
  if (na.rm) x <- na.omit(x)
  tx  <- table(x)
  ret <- list(mean=mean(x),
              median=median(x),
              harmonic=harmonic.mean(x), 
              geometric=exp(mean(log(x))),
              mode=as.numeric(names(tx)[which.max(tx)]))
  if (!is.na(trim))   ret$trim <- mean(x, trim=trim)
  if (!is.na(winsor)) ret$winsor <- winsor.mean(x, trim=winsor)
  attr(ret, "mindiff") <- min(diff(sort(unlist(ret))))
  for (i in 1:length(ret)) ret[[i]] <- round(ret[[i]], digits)
  ret
}

#' @rdname means_choice
#' @export
means <- function (x, digits, na.rm=TRUE, trim=0.2, winsor=0.2) {
  means_choice(x=x, digits=digits, na.rm=na.rm, trim=trim, winsor=winsor)
}
