#' @rdname makekey
#' @aliases make_key
#' @title Character Key Generation
#' @description Generates a character key from a vector of integers.
#'
#' @param index integer: vector of integer
#'
#' @return A character.
#' @export
#'
#' @examples
#' makekey(1)
#' makekey(1:2)
#' makekey(pi) # ;)
#' makekey(c(5,4))
makekey <- function(index) { 
  toString(as.integer(index)) 
}

#' @rdname makekey
#' @export
# make_key <- function(...){
#  makekey(...)}
make_key <- makekey
