#' List Package Files Matching a Pattern
#'
#' Retrieves a list of file paths within a specified package that match a given pattern. 
#' By default, it delivers all file names in the `exams.forge` package.
#'
#' @param pattern character: regular expression to match file names (default: `NULL` lists all files of the package)
#' @param package character: specifying the name of the package (default: `"exams.forge"`)
#'
#' @return A character vector of file paths matching the specified pattern within the 
#'   specified package.
#'
#' @details The function uses [list.files()] to search the file system. 
#'   The search is performed in the package's installed directory, as determined by [system.file()].
#'   
#' This documentation was created with the support of ChatGPT.
#'
#' @examples
#' # List all files in the exams.forge package
#' pkg_files()
#' # List all files with 'exam.rmarkdown' in their name in the exams.forge package
#' pkg_files("exam.rmarkdown")
#' # List all files in another package
#' pkg_files(package = "polynom")
#' 
#' @export
pkg_files <- function(pattern=NULL, package="exams.forge") {
  files <- list.files(path=system.file(package=package), recursive = TRUE, full.names = TRUE)
  if (!is.null(pattern)) files <- files[grepl(pattern, files)]
  files
}