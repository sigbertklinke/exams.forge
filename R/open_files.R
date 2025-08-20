#' Open Multiple Files with Optional Menu Selection
#'
#' The `open_files` function allows users to manage multiple files by either opening them interactively 
#' or reading their contents programmatically. It supports interactive selection through a menu 
#' for multiple file paths and adapts to the availability of the `rstudioapi` package for enhanced 
#' integration with the RStudio IDE.
#'
#' @param file character: vector of file paths, where each path specifies the location of a file 
#' to be opened or read; paths may be absolute or relative
#' @param open logical: Whether to open the files interactively (`TRUE`) or read their contents 
#' programmatically (`FALSE`). Defaults to the result of [interactive()].
#' @param ... Additional arguments to be passed to the file opening or editing function; 
#' these arguments are passed either to [rstudioapi::navigateToFile()] or [utils::edit()], 
#' depending on the environment and the availability of the `rstudioapi` package.
#'
#' @details
#' When the `file` argument contains more than one file path, the function calculates the 
#' longest common subsequence (LCS) of the directory structures for all provided file paths 
#' and presents the user with an interactive menu. The menu allows selection of files to open; 
#' the options include "All," "None," or individual files that match the common directory pattern. 
#' In non-interactive sessions, the function reads the content of the files instead.
#'
#' Internally, the function determines whether the `rstudioapi` package is available, in which case 
#' it uses `navigateToFile` for opening files within the RStudio IDE; otherwise, it falls back to the 
#' base R function `edit`. In non-interactive sessions, the function reads file contents using 
#' `readLines` and stores them in a named list, with filenames as the names of the list elements.
#'
#' The auxiliary `lcss` function calculates the longest common subsequence of directory components 
#' for the file paths; this helps in simplifying the user experience by grouping files with similar paths.
#' 
#' This documentation was created with the support of ChatGPT.
#' 
#' @return
#' Returns an invisible named list. Each element corresponds to a file from the input vector, with the 
#' value representing the result of the operation. When opening files, the result indicates success or failure. 
#' When reading files, the result contains the file's contents. If "None" is selected, the function returns `NULL`.
#'
#' @examples
#' if (interactive()) {
#'   files <- pkg.files("poylnomials", package="polynom")
#'   open_files(files[2]) # open one file
#'   open_files(files)    # open several files
#' }
#'
#' @export
#' @importFrom rstudioapi navigateToFile
#' @importFrom utils edit
open_files <- function (file, open=interactive(), ...) {
  lcss <- function(file) {
    f <- strsplit(file, "[\\/]")
    for (i in 1:min(lengths(f))) {
      if (length(unique(sapply(f, '[', i)))>1) break
    }
    if (i==1) return(file)
    sapply(f, p=i-1, function(v, p) {
      paste0(v[-(1:p)], collapse="/")
    })
  }
  #
  if (open && (length(file)>1)) {
    f   <- lcss(file)
    res <- select_menu(c("All", "None", f), title="Select one or more files to open:")
    if ("None" %in% res || length(res) == 0) return(NULL)
    if (!("All" %in% res)) file <- file[f %in% res]
  }
  #
  res <- list()
  if (open) {
    for (f in file) {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        res[[f]] <- try(navigateToFile(file = f, ...), silent = TRUE)
      } else {
        res[[f]] <- try(edit(file = f, ...), silent = TRUE)        
      }
    }
  } else {
    for (f in file) res[[f]] <- try(readLines(f), silent = TRUE)
  }
  invisible(res)
}