#' exams.forge: A brief description the package
#'
#' The `exams.forge` package was created with the main goal of "forging" exam 
#' tasks in combination with the `exams` package, and it includes additional 
#' functions to simplify the creation of Moodle exercises. 
#' The package features various functions categorized into seven groups based on 
#' their characteristics. These categories are named: 
#' Data Conversion and Modification, Statistical Analysis, 
#' Mathematical Computations, Exercise Generation, String Manipulation, 
#' LaTeX and HTML Functions, and General Purpose Functions.
#'
#' This package is designed for educators who need to develop examination materials 
#' in the field of statistics, particularly for introductory courses like Statistics I and II, 
#' using the R programming language. The aim is to streamline the process of creating a 
#' large number of assessment items, enabling instructors to focus on improving the quality 
#' of the tasks themselves.
#' 
#' We would like to acknowledge the support provided by the Multimedia Funding Program. 
#' Their assistance has been invaluable to our project, and we extend our sincere gratitude 
#' for their contributions.
#'
#' @section Features of the package:
#' - Feature 1: `exams.forge` simplifies the generation of examination tasks by providing tools to 
#' create a diverse array of statistical exercises, ensuring unique problem sets for each student.
#' - Feature 2:  It includes functions for precise data conversion, statistical analysis, 
#' and mathematical computations, enhancing the accuracy and relevance of generated exercises.
#' - Feature 3: The package supports multi-format rendering, allowing the seamless creation of 
#' LaTeX and HTML documents suitable for various educational platforms, such as Moodle.
#'
#' @section Functions:
#' Examples of functions included in the package:
#' \itemize{
#'   \item \code{\link{ts_data}}: Creates a univariate time series by combining elements of a 
#'   linear or exponential trend, additive or multiplicative seasonal adjustment, and white noise. 
#'   The resulting time series is structured as a `ts_data object`, allowing for further analysis and exploration.
#'   \item \code{\link{lmatrix}}: Creates a LaTeX or HTML representation of a matrix. 
#'   This function is useful for integrating well-formatted matrices into LaTeX or HTML documents.
#'   \item \code{\link{as_obs}}: Creates a string representing observations with optional sorting 
#'   and LaTeX formatting, useful for generating readable data representations in educational materials.
#' }
#'
#' @section Usage:
#' Example usage of the package and its functions.
#' 
#' \code{
#' library(exams.forge)
#' # Generate a time series with specified parameters
#' ts_eg <- ts_data(end = 20, trend = TRUE, trend.coeff = c(1, 0.5), season = TRUE, season.coeff = c(0.2, 0.1), error = TRUE, error.coeff = 0.1, digits = 2)
#' print(ts_eg)
#' }
#' 
#' \code{
#' # Create a matrix
#' mx_data <- matrix(1:6, ncol = 2)

#' # Generate a LaTeX representation of the matrix with tooltip
#' eg_matrix <- lmatrix(
#'  m = mx_data,
#'  title = "Example LaTeX Matrix",
#'  fmt = "%.2f",
#'  byrow = TRUE,
#'  tooltip = "Die Tabelle hat %.0f Zeilen und %.1f Spalten")
#' cat(eg_matrix)
#' }
#'
#'\code{
#' # Create a string representation of observations
#' observations <- c(10, 20, 30, 40, 50)
#' observation_string <- as_obs(observations, last = " and ")
#' print(observation_string)
#' }
#'
#' @section Installation:
#' To install this package please use the following command:
#' \code{install.packages("exams.forge")}
#'
#' @section Author(s):
#' Sigbert Klinke, Affiliation: Humboldt University of Berlin, School of Business and Economics, Chair of Statistics.
#'
#' @section Maintainer:
#' Sigbert Klinke \email{sigbert@wiwi.hu-berlin.de}
#'  
#' @section License:
#' Gnu General Public License 3.
"_PACKAGE"