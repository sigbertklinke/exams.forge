#' @rdname add_data
#' @title Add Random Data to the Ends of a Vector
#'
#' @description
#' Generates and appends additional random values to the left and/or right ends of a numeric vector \code{x}.
#' The range from which these values are drawn is determined by a specified "box" and a scaling factor.
#'
#' @details
#' The "box" defines the central range of the data and can be:
#' \itemize{
#'   \item \code{"boxplot"} — uses the 25th and 75th percentiles (\code{quantile(x, c(0.25, 0.75), na.rm = TRUE)}).
#'   \item \code{"range"} — uses the full range of the data (\code{range(x, na.rm = TRUE)}).
#'   \item A numeric vector of length two — used directly as the box boundaries.
#' }
#'
#' The \strong{box length} is the distance between the lower and upper box boundaries.
#' The \code{range} parameter specifies how far left or right new values can be drawn,
#' as a multiple of the box length.
#'
#' For the left side, values are drawn uniformly from:
#' \deqn{[ \text{box[1]} - \text{range[2]} \times \text{box length},\; \text{box[1]} - \text{range[1]} \times \text{box length} ]}
#'
#' For the right side, values are drawn uniformly from:
#' \deqn{[ \text{box[2]} + \text{range[1]} \times \text{box length},\; \text{box[2]} + \text{range[2]} \times \text{box length} ]}
#'
#' The \code{n} parameter controls how many values are added:
#' \itemize{
#'   \item Single number — adds that many values to the right side only.
#'   \item Length-two vector — \code{n[1]} values to the left, \code{n[2]} values to the right.
#' }
#'
#' @param x \code{numeric}  
#'   Data vector to which new values will be added.
#' @param box \code{character} or \code{numeric}  
#'   Defines the base range ("box") used for value generation. See \strong{Details}.
#' @param n \code{numeric}  
#'   Number of values to add to each side of \code{x}. Defaults to \code{c(0, 1)}.
#' @param range \code{numeric}  
#'   Multiplicative factors of box length for determining value generation intervals. Defaults to \code{c(0, 1)}.
#'
#' @return
#' A numeric vector containing the original values from \code{x} plus the newly generated values.
#'
#' @importFrom stats quantile runif
#' @export
#'
#' @examples
#' x <- rnorm(8)
#'
#' # Add one value to the right
#' add_data(x, "box", range = 1.5)
#'
#' # Add one value to the right using data range
#' add_data(x, "range", range = 0.1)
#'
#' # Add one value to the right, larger possible range
#' add_data(x, "box", range = c(1.5, 3))
#'
#' # Add two values to the right
#' add_data(x, "range", n = 2, range = 0.1)
#'
#' # Add two values to the left and three to the right
#' add_data(x, "range", n = c(2, 3), range = 0.1)
add_data <- function(x, box, n=c(0,1), range=c(0,1)) {
  if (missing(box)) stop("parameter 'box' is required")  
  if (length(n)==1) n <- c(0,n)
  if (is.character(box)) {
    boxi <- pmatch(box, c("boxplot", "range")) 
    stopifnot(is.finite(boxi))
    if (boxi==1) box <- quantile(x, c(0.25, 0.75), na.rm=TRUE)
    if (boxi==2) box <- range(x, na.rm=TRUE)
  }
  box   <- range(box)
  blen  <- diff(box)
  rangeval <- range(range)
  if (length(n)>1) {
    xp <- c(runif(n[1], min=box[1]-blen*rangeval[2], max=box[1]-blen*rangeval[1]),
           runif(n[2], min=box[2]+blen*rangeval[1], max=box[2]+blen*rangeval[2]))
  } else {
    xp <- runif(n[1], min=box[1]-blen*rangeval[2], max=box[1]-blen*rangeval[1])
  }
  c(x, xp)
}
