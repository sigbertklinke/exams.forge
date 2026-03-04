#' Discrete uniform distribution
#'
#' Probability mass function, distribution function, quantile function and random generation
#' for the discrete uniform distribution. Documentation, examples and interface are taken from 
#' \code{\link[extraDistr:DiscreteUniform]{extraDistr::DiscreteUniform}}.
#'
#' @param x,q	            vector of quantiles.
#' @param p	              vector of probabilities.
#' @param n	              number of observations. If \code{length(n) > 1},
#'                        the length is taken to be the number required.
#' @param min,max         lower and upper limits of the distribution. Must be finite.
#' @param log,log.p	      logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail	    logical; if TRUE (default), probabilities are \eqn{P[X \le x]}
#'                        otherwise, \eqn{P[X > x]}.
#' 
#' @details 
#' 
#' If \code{min == max}, then discrete uniform distribution is a degenerate distribution.
#'                                           
#' @examples 
#' x <- rdunif(1e5, 1, 10) 
#' xx <- -1:11
#' plot(prop.table(table(x)), type = "h")
#' lines(xx, ddunif(xx, 1, 10), col = "red")
#' hist(pdunif(x, 1, 10))
#' xx <- seq(-1, 11, by = 0.01)
#' plot(ecdf(x))
#' lines(xx, pdunif(xx, 1, 10), col = "red")
#'
#' @name DiscreteUniform
#' @aliases DiscreteUniform
#' @aliases ddunif
#' 
#' @keywords distribution
#'
#' @export

# Probability Mass Function (Wahrscheinlichkeitsfunktion)
ddunif <- function(x, min, max, log = FALSE) {
  # min und max müssen ganze Zahlen sein, x ebenfalls für P > 0
  p <- ifelse(x >= min & x <= max & x == floor(x), 1 / (max - min + 1), 0)
  if (log) return(log(p))
  return(p)
}

# Distribution Function (Verteilungsfunktion)
#' @rdname DiscreteUniform
#' @export
pdunif <- function(q, min, max, lower.tail = TRUE, log.p = FALSE) {
  q_floor <- floor(q)
  p <- (q_floor - min + 1) / (max - min + 1)
  
  # Werte außerhalb der Grenzen korrigieren
  p[q < min] <- 0
  p[q > max] <- 1
  
  if (!lower.tail) p <- 1 - p
  if (log.p) return(log(p))
  return(p)
}

# Quantile Function (Quantilsfunktion)
#' @rdname DiscreteUniform
#' @export
qdunif <- function(p, min, max, lower.tail = TRUE, log.p = FALSE) {
  # 1. Parameter-Checks (Vektorisierung von min/max beachten)
  if (any(is.na(min)) || any(is.na(max))) return(rep(NaN, length(p)))
  # 2. Wahrscheinlichkeiten berechnen (Log-Skala berücksichtigen)
  p_lin <- if (log.p) exp(p) else p
  if (!lower.tail) p_lin <- 1 - p_lin
  # 3. Ergebnisvektor mit NaNs initialisieren
  q <- rep(NaN, max(length(p_lin), length(min), length(max)))
  # 4. Maske für gültige Eingaben (p im Intervall [0, 1])
  ok <- !is.na(p_lin) & p_lin >= 0 & p_lin <= 1
  # 5. Warnung bei ungültigen Werten (wie bei qunif, qnorm etc.)
  if (any(!ok & !is.na(p_lin))) {
    warning("NaNs produced")
  }
  # 6. Eigentliche Berechnung nur für gültige Indizes
  q[ok] <- ceiling(p_lin[ok] * (max[ok] - min[ok] + 1) + min[ok] - 1)
  # 7. Exakte Randwerte (Präzisions-Fix)
  q[p_lin == 0] <- min[p_lin == 0]
  q[p_lin == 1] <- max[p_lin == 1]
  return(q)
}

# Random Generation (Zufallswerte)
#' @rdname DiscreteUniform
#' @export
rdunif <- function(n, min, max) {
  if (length(n) > 1) n <- length(n)
  # R's sample Funktion macht genau das für die diskrete Gleichverteilung
  return(sample(min:max, size = n, replace = TRUE))
}