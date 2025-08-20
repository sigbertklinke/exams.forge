#' @title Quote, Bracket, and Prefix/Suffix String Manipulation
#' @description A set of helper functions for adding or removing specific prefixes and/or suffixes
#' to character vectors. This is useful for formatting strings in mathematical,
#' XML, or other structured text contexts.
#'
#' - `affix()` – Add any prefix and/or suffix to each element of a character vector; alias: `add_affix()`.
#' - `math()` – Add a dollar sign (`$`) to both ends of each element (LaTeX-style math); aliases: `add_math()`, `lmath()`.
#' - `bracket()` – Wrap each element in parentheses; aliases: `add_bracket()`, `brkt()`.
#' - `cdata()` – Wrap each element in `<![CDATA[ ... ]]>` (XML CDATA section); alias: `add_cdata()`.
#' - `unaffix()` – Remove a given prefix and/or suffix from each element; alias: `remove_affix()`.
#' - `unquote()` – Remove surrounding double quotes from each element; alias: `remove_quotes()`.
#' - `uncdata()` – Remove a surrounding CDATA section from each element.; alias: `remove_cdata()`.
#'
#' @name affix
#' @aliases add_affix add_math lmath add_bracket brkt add_cdata
#' @aliases remove_affix remove_quotes remove_cdata

#' @param txt `character` vector to modify.
#' @param prefix `character(1)` Prefix to add or remove. Default is `""`.
#' @param suffix `character(1)` Suffix to add or remove. Default is `""`.
#'
#' @return A modified `character` vector of the same length as `txt`.
#' @export
#'
#' @examples
#' x <- c("alpha", "beta", "gamma")
#'
#' # Add a prefix and suffix
#' affix(x, "[", "]")
#' #> [1] "[alpha]" "[beta]" "[gamma]"
#'
#' # Wrap with LaTeX math delimiters
#' math(x)
#' #> [1] "$alpha$" "$beta$" "$gamma$"
#'
#' # Remove quotes
#' quoted <- c('"a"', '"b"')
#' unquote(quoted)
#' #> [1] "a" "b"
#'
#' # Wrap and unwrap CDATA
#' cdata_x <- cdata("text")
#' uncdata(cdata_x)
affix <- function(txt, prefix='', suffix='')  { 
  paste0(prefix, txt, suffix) 
}

#' @rdname affix
# @description \code{math} adds a \code{$} as pre- and suffix to a (character) vector
#' @export
math  <- function(txt) { 
  affix(txt, '$', '$') 
}

#' @rdname affix
# @description \code{bracket} adds a \code{(} as prefix and \code{)} as suffix to a (character) vector
#' @export
bracket <- function(txt) { 
  affix(txt, '(', ')') 
}

#' @rdname affix
# @description \code{unaffix} deletes a pre- and/or suffix to a (character) vector
#' @export
unaffix  <- function(txt, prefix='', suffix='')  { 
  index <- which(startsWith(txt, prefix))
  if (length(index)) txt[index] <- substring(txt[index], nchar(prefix)+1)
  index <- which(endsWith(txt, suffix))
  if (length(index)) txt[index] <- substring(txt[index], 1, nchar(txt)-nchar(suffix))
  txt
}

#' @rdname affix
# @description \code{unquote} deletes double quotes at the beginning and the ending of a (character) vector
#' @export
unquote  <- function(txt) { unaffix(txt, '"', '"') }
  
#' @rdname affix
# @description \code{uncdata} deletes a \code{<![CDATA[} as prefix and \code{]]>} as suffix
#' @export  
uncdata  <- function(txt) { unaffix(txt, '<![CDATA[', ']]>') }

#' @rdname affix
# @description \code{cdata} adds a \code{<![CDATA[} as prefix and \code{]]>} as suffix
#' @export  
cdata  <- function(txt) { affix(txt, '<![CDATA[', ']]>') }

#' @rdname affix
#' @export
# add_affix <- function(...){
#  affix(...)}
add_affix <- affix

#' @rdname affix
#' @export
# add_cdata <- function(...){
#  cdata(...)}
add_cdata <- cdata

#' @rdname affix
#' @export
# add_math <- function(...){
# math(...)}
add_math <- math

#' @rdname affix
#' @export
# lmath <- function(...){
#  math(...)}
lmath <- math

#' @rdname affix
#' @export
# add_bracket <- function(...){
#  bracket(...)}
add_bracket <- bracket

#' @rdname affix
#' @export
# brkt <- function(...){
#  bracket(...)}
brkt <- bracket

#' @rdname affix
#' @export
# remove_affix <- function(...){
#  unaffix(...)}
remove_affix <- unaffix

#' @rdname affix
#' @export
# remove_quotes <- function(...){
#  unquote(...)}
remove_quotes <- unquote

#' @rdname affix
#' @export
# remove_cdata <- function(...){
#  uncdata(...)}
remove_cdata <- uncdata

