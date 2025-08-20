#' @rdname nsprintf
#' @aliases round_de
#' @aliases schoice_de
#' @aliases print_de
#' @title sprintf with template depending on integer valued `n`
#' @description 
#' `nsprintf` creates a text dependent on the value(s) in `n`. In particular, we have
#'  * `round_de`, it returns either `Runden Sie Ihr Ergebnis auf eine ganze Zahl`,
#' `Runden Sie Ihr Ergebnis auf eine Stelle nach dem Komma`,  or 
#' `Runden Sie Ihr Ergebnis auf n Stellen nach dem Komma`
#'  * `schoice_de` returns `Es kann eine oder mehrere Antworten richtig sein. Es ist ausreichend, eine richtige Antwort anzugeben. Geben Sie mehrere Antworten an und eine ist falsch, dann ist die Aufgabe falsch beantwortet`
#' 
#' @param n integer: number(s) to be used
#' @param ... character: format strings to be used 
#'
#' @return \code{sprintf}ed strings
#' @export
#'
#' @examples
#' nsprintf(0, '0' = "keine Netzunterbrechung", '1' = "eine Netzunterbrechung", 
#'             "%i Netzunterbrechungen")
#' nsprintf(0:3, `0` = "keine Netzunterbrechung", `1` = "eine Netzunterbrechung", 
#'               "%i Netzunterbrechungen")
nsprintf <- function(n, ...) {
  n     <- as.integer(n)
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep("", length(args))
  if (anyDuplicated(nargs)) stop("Duplicated names are not allowed")
  pos <- match(as.character(n), nargs)
  pos[is.na(pos)] <- which(nargs=="")
  suppressWarnings(sprintf(unlist(args[pos]), n))     
}

#' @rdname nsprintf
#' @importFrom stranslate getMsg
#' @export
round_de <- function(n) {
  nsprintf(n, '0'='Runden Sie Ihr Ergebnis auf eine ganze Zahl',
              '1'='Runden Sie Ihr Ergebnis auf eine Stelle nach dem Komma',
              'Runden Sie Ihr Ergebnis auf %i Stellen nach dem Komma')
#  getMsg(ROUND=n, .domain="exams.forge")
}

#' @rdname nsprintf
#' @importFrom stranslate getMsg
#' @export
schoice_de <- function() {
#  getMsg('CHOICE', .domain="exams.forge")
  "Es kann eine oder mehrere Antworten richtig sein. Es ist ausreichend, eine richtige Antwort anzugeben. Geben Sie mehrere Antworten an und eine ist falsch, dann ist die Aufgabe falsch beantwortet"
}

#' @rdname nsprintf
#' @export
print_de <- nsprintf
