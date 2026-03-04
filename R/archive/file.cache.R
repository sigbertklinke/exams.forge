#' Cache and Optionally Decompress a File
#'
#' Downloads or copies a file to a cache directory, either in a temporary
#' location or in a user-specific persistent directory. If the file is
#' compressed, it can optionally be decompressed using the \pkg{archive}
#' package.
#'
#' By default, the file is stored under \code{tempdir()}. If
#' \code{use_temp = FALSE}, the file may instead be cached under
#' \code{R_user_dir(package, "data")}, with user confirmation if the directory
#' does not yet exist and the session is interactive.
#'
#' If \code{decompress = TRUE}, compressed archives (e.g. \code{.zip},
#' \code{.tar}, \code{.tar.gz}, \code{.tgz}, \code{.gz}) are extracted into a
#' subdirectory named after the file (without extension), and the paths of the
#' extracted files are returned.
#'
#' @param file Character scalar. A local file path or a URL.
#' @param package Character scalar. The package name used to construct
#'   the persistent cache path.
#' @param decompress Logical. Whether to decompress the file if it is an
#'   archive. Defaults to \code{TRUE}.
#' @param use_temp Logical. If \code{TRUE} (default), use a temporary cache
#'   directory under \code{tempdir()}. If \code{FALSE}, attempt to use a
#'   persistent cache directory under \code{R_user_dir()}.
#' @param overwrite Logical. Whether to overwrite an existing cached file.
#'   Defaults to \code{FALSE}.
#' @param quiet Logical. If \code{TRUE}, suppress progress and informative
#'   messages. By default, \code{quiet = !interactive()}.
#'
#' @return A character vector of file paths:
#'   \itemize{
#'     \item If \code{decompress = FALSE}, a length-1 character vector with the
#'       path to the cached file.
#'     \item If \code{decompress = TRUE} and the file is an archive, a character
#'       vector with the paths of the extracted files.
#'   }
#'
#' @importFrom tools R_user_dir file_path_sans_ext
#' @importFrom archive archive_extract
#' @export
file.cache <- function(file, package, decompress = TRUE, use_temp = TRUE,
                       overwrite = FALSE, quiet = !interactive()) {
  stopifnot(length(file) == 1, length(package) == 1)
  bname <- basename(file)
  # Directories: temporary vs persistent
  tdir   <- c(file.path(tempdir(), package), R_user_dir(package, "data"))
  tfile  <- file.path(tdir, bname)
  target <- 1
  # Select persistent cache if allowed
  if (isFALSE(use_temp)) {
    if (!dir.exists(tdir[2])) {
      if (interactive()) {
        ans <- readline(paste0("Create persistent data directory (", tdir[2], ")? [y/N]: "))
        if (tolower(ans) == "y") target <- 2
      } else {
        if (!quiet) message("Persistent cache unavailable in non-interactive mode, using tempdir().")
      }
    } else {
      target <- 2
    }
  }
  # Ensure target directory exists
  if (!dir.exists(tdir[target])) dir.create(tdir[target], recursive = TRUE, showWarnings = FALSE)
  # Download or copy file if not present or overwrite = TRUE
  if (!file.exists(tfile[target]) || overwrite) {
    if (!quiet) message("Caching file to: ", tfile[target])
    if (grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", file)) {
      utils::download.file(file, tfile[target], mode = "wb", quiet = quiet)
    } else {
      file.copy(file, tfile[target], overwrite = overwrite)
    }
  }
  ret <- tfile[target]
  # Optionally decompress using archive
  if (decompress) {
    edir   <- file.path(dirname(tfile[target]), tools::file_path_sans_ext(bname))
    efiles <- tryCatch({
      dir.create(edir, recursive = TRUE, showWarnings = FALSE)
      archive::archive_extract(tfile[target], dir = edir)
    }, error = function(e) {
      unlink(edir, recursive = TRUE) 
      NULL
    })
    if (!is.null(efiles) && length(efiles) > 0) {
      ret <- normalizePath(efiles, mustWork = FALSE)
    }
  }
  ret
}
