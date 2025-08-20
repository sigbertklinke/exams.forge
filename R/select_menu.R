#' Display a Menu for User Selection
#'
#' Presents a list of choices to the user, allowing them to select one or more options by entering numbers. 
#' If the user inputs an empty line, the function skips the selection and returns `NULL`.
#'
#' @details
#' This documentation was created with the support of ChatGPT.
#' 
#' @seealso Based on [`select_menu()` in the package `remotes`](https://github.com/r-lib/remotes/blob/main/R/deps.R)
#' 
#' @param choices character: vector of options to display to the user. Each choice will be prefixed with a numbered label.
#' @param title character: optional string to display as the title of the menu. Default is `NULL`, meaning no title is displayed.
#' @param msg character: string specifying the prompt message for user input. Default is `"Enter one or more numbers, or an empty line to open files: "`.
#' @param width integer: specifying the maximum line width for displaying the menu. Defaults to the value of the `width` option (`getOption("width")`).
#' 
#' @return A character vector of selected choices, or `NULL` if the user enters an empty line.
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' options <- c("Apple", "Banana", "Cherry", "Date")
#' selected <- select_menu(options, title = "Choose your fruits:")
#' cat("You selected:", paste(selected, collapse = ", "), "\n")
#' }
#'
#' @export
select_menu <- function(choices, title = NULL, msg = "Enter one or more numbers, or an empty line to open files: ", width = getOption("width")) {
  if (!is.null(title)) {
    cat(title, "\n", sep = "")
  }
  
  nc <- length(choices)
  op <- paste0(format(seq_len(nc)), ": ", choices)
  fop <- format(op)
  cat("", fop, "", sep = "\n")
  repeat {
    answer <- readline(msg)
    answer <- strsplit(answer, "[ ,]+")[[1]]
    if (all(answer %in% seq_along(choices))) {
      return(choices[as.integer(answer)])
    }
  }
}