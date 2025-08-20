#' @title Retrieve and Cache Data from an External Package
#' @aliases get_data
#'
#' @description Dynamically loads a dataset from an external package and caches it in the package's environment for future use.
#'
#' @param name character: name of the data set to be retrieved
#' @param package character: package from which to load the data set (default: \code{"exams.forge.data"}).
#'
#' @details This function checks if the dataset specified by \code{name} is already cached in the package's environment. 
#' If it is not cached, the function loads the dataset using the \code{data()} function and stores it in the package environment. 
#' Subsequent calls to the function with the same dataset name will use the cached version.
#'
#' @return The requested dataset, loaded into and retrieved from the package environment.
#' @export
#'
#' @examples
#' dist_data <- get_data("distributions")
#' head(dist_data)
get_data <- function(name, package="exams.forge.data") {
  browser()
  if(is.null(the[[name]])) data(list=name, package=package, envir=the)
  the[[name]]
}