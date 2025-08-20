histwidthint <- function(len, widths, min=3, prob = NULL) {
  len    <- as.integer(len)
  stopifnot(len>0)
  widths <- as.integer(widths)
  stopifnot(all((widths>0) & (widths<len)))
  slen   <- len/min(widths) 
  repeat {
    w <- sample(widths, slen, replace=TRUE, prob=prob)
    i <- which(cumsum(w)==len) 
    if (length(i)) {
      if (i>=min) {
        w <- w[1:i]
        break
      }   
    }
  }
  w
}

histwidth2 <- function(from, to, widths, dmax = 100) {
  w    <- c(to-from, widths)
  wmin <- min(w)
  i    <- 1
  while(i<120) {
    wi <- wmin/i
    ws <- round(w/wi,0) 
    if (all(abs(ws-round(ws))<1e-9)) {
      ws <- as.integer(ws)
      break
    }
    i  <- i+1
  }
  stopifnot(i<120)
  w   <- histwidthint(ws[1], ws[-1])
  ret <- list(breaks=from+c(0, cumsum(w))*wi)
  ret
}