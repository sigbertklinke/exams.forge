#' Clean or Schedule Deletion of a File
#'
#' Deletes a file immediately after a specified delay or schedules it for deletion
#' when the R session ends.
#'
#' @param file Character string. Path to the file to be deleted.
#' @param delay Numeric. Number of seconds to wait before deleting the file. 
#'   If `delay = 0`, the file will persist until the R session ends.
#'
#' @return Invisibly returns the file path.
#' @export
#'
#' @examples
#' { # Delete a temporary file after 2 seconds
#'   tmp <- tempfile(); file.create(tmp); cleanFile(tmp, delay = 2)
#'   # Keep a temporary file until the R session ends
#'   tmp2 <- tempfile(); file.create(tmp2); cleanFile(tmp2, delay = 0)
#' }

cleanFile <- function(file, delay) {
  if (delay > 0) {
    Sys.sleep(delay)
    if (file.exists(file)) unlink(file)
  } else {
    # Keep file until R session ends
    reg.finalizer(baseenv(), function(e) {
      if (file.exists(file)) unlink(file)
    }, onexit = TRUE)
  }
}