#' Recursive Parsing of Expressions
#'
#' The `vexpar` function extends the functionality of `expar`, enabling recursive expression parsing for multiple inputs.
#' Unlike `expar`, which operates on a single file name, 
#' `vexpar` can process either a single file name, a vector file names or a list of file names, 
#' applying the parsing operation to each element automatically.
#'
#' @param file A single file (character) or a list of files (character vector) to be parsed.
#' If `file` is a list, the function will recursively call itself for each element in the list.
#' @param ... Additional arguments to be passed to the underlying [exams::expar()] function.
#'
#' @return If `file` is a list, the function returns a list where each element is the result of parsing a file in the list. If `file` is a character vector, it returns a vector of parsed expressions (or objects) obtained by applying `expar` to each file.
#' @details
#' - When a list is provided, the function uses `lapply` to handle each element.
#' - For a character vector, the function applies `sapply` to parse each file using the [exams::expar()] function.
#' 
#' This function serves as a recursive wrapper for the [exams::expar()] function.
#'
#' This documentation was created with the support of ChatGPT.
#'
#' @examples
#' \dontrun{
#' library("exams")
#' # Example usage with a single file
#' vexpar("file1.Rmd", instruction=NULL)
#' # Example usage with multiple files in a character vector
#' vexpar(c("file1.Rmd", "file2.Rmd"), instruction=NULL)
#' # Example usage with a list of files
#' vexpar(list("file1.Rmd", "file2.Rmd"), instruction=NULL)
#' }
#' 
#' @importFrom exams expar
#' @export
vexpar <- function(file, ...) {
  if (is.list(file)) return(lapply(file, vexpar, ...))
  sapply(file, expar, ...)
}
