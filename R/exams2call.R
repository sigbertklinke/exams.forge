#' @rdname exams2call
#' @title Traceback for `exams2` Functions
#' @description Returns a list with the functions' names and parameters called from \code{.traceback()}. 
#' The function name must start with \code{"exams2"}.
#'
#' @param prefix character: start of the function name (default: \code{"exams2"})
#'
#' @return A list with the function name and its evaluated parameters.
#' @export
#'
#' @examples
#' \donttest{
#'    exams2call()                 # access current call stack
#' }
exams2call <- function(prefix = "exams2") {
  # Anzahl der Frames im aktuellen Stack
  nframes <- sys.nframe()
  # von aktuell nach global iterieren
  for (i in nframes:1) {
    call_i <- sys.call(i)   # aktueller Call im Frame
    if (!is.null(call_i)) {
      cmd <- as.character(call_i[[1]])
      if (length(cmd) == 1 && startsWith(cmd, prefix)) {
        
        # Argumente des Calls als Liste
        ret <- as.list(match.call(match.fun(cmd), call_i))
        ret[[1]] <- cmd  # Name der Funktion als String
        
        # Argumente auswerten
        if (length(names(ret))) {
          for (name in names(ret)) {
            if (is.symbol(ret[[name]]) || is.language(ret[[name]])) {
              # evaluiere im entsprechenden Frame
              ret[[name]] <- eval(ret[[name]], envir = sys.frame(i))
            }
          }
        }
        return(ret)  # sofort zurückgeben, erstes Match
      }
    }
  }
  
  # Wenn kein passender Call gefunden wurde
  return(NULL)
}
