#' Retrieve or Compute Sum-of-Squares Dataset
#'
#' The function \code{sos()} retrieves a dataset of the form \code{sosN}, where
#' \code{N} is the input parameter \code{n}. The function tries several sources
#' in order:
#' \enumerate{
#'   \item Load from the package \code{exams.forge} if available.
#'   \item Load from the user data directory (see \code{\link[tools]{R_user_dir}}).
#'   \item Download from a GitHub repository of `exams.forge`.
#'   \item Compute the dataset on the fly using \code{sumofsquares()}.
#' }
#'
#' In steps 3 and 4, the dataset is cached in the user data directory (compressed
#' with \code{xz}). If the directory does not exist and the session is interactive,
#' the user is prompted for permission to create it; otherwise a temporary
#' directory is used. In non-interactive sessions, the directory is used only
#' if it already exists, otherwise \code{tempdir()} is used silently.
#'
#' @param n Integer, specifying the dataset index (e.g., \code{n = 3} returns \code{sos3}).
#' @param nmax Integer, the maximum value passed to \code{sumofsquares()}.
#'   Defaults to \code{10}.
#' @param quiet Logical. If \code{FALSE}, progress messages are printed.
#'   Defaults to \code{!interactive()}.
#' @param download Logical. If \code{TRUE}, the function attempts to download the data 
#'   from the GitHub repository of `exams.forge`. Defaults to \code{interactive()}.
#'
#' @return An \R object named \code{sosN}, corresponding to the requested dataset.
#'   If no solution exists for the given \code{n}, a matrix with a single row
#'   filled with \code{NA} values is returned.
#'
#' @details
#' The computation step calls:
#' \preformatted{
#'   sumofsquares(n, nmax, maxt = Inf, zerosum = TRUE)
#' }
#'
#' @seealso \code{\link[tools]{R_user_dir}}, \code{\link{sumofsquares}}
#'
#' @examples
#' x100 <- sos(100) # part of the package
#' rowSums(x100, na.rm=TRUE)     # only zeros
#' rowSums(x100^2, na.rm=TRUE)   # only 100's
#' x10 <- sos(10)   # must be computed
#' rowSums(x10, na.rm=TRUE)     # only zeros
#' rowSums(x10^2, na.rm=TRUE)   # only 144's
#'
#' @importFrom tools R_user_dir
#' @importFrom utils download.file data
#' @importFrom httr HEAD
#' @export
sos <- function(n, nmax = 10, quiet = !interactive(), download = interactive()) {
  #browser()
  dataset_name <- paste0("sos", n)
  
  # 1. Check package data first
  if (exists(dataset_name, where = "package:exams.forge")) {
    if (!quiet) message("Loading dataset from package: ", dataset_name)
    data(list = dataset_name, package = "exams.forge", envir = environment())
    ret <- get(dataset_name, envir = environment())
    return(ret)
  }
  
  # 2. Determine user data directory
  data_dir <- the$data_dir
  if (is.null(data_dir)) {
    data_dir <- R_user_dir("exams.forge", "data")
    if (!dir.exists(data_dir)) {
      if (interactive()) {
        if (!quiet) message("User data directory does not exist: ", data_dir)
        ans <- readline(paste0("Do you want to create it at ", data_dir, " ? [y/n]: "))
        if (tolower(ans) == "y") {
          dir.create(data_dir, recursive = TRUE)
          if (!quiet) message("Created user data directory: ", data_dir)
        } else {
          data_dir <- tempdir()
          if (!quiet) message("Using temporary directory instead: ", data_dir)
        }
      } else {
        data_dir <- tempdir()
        if (!quiet) message("Non-interactive session: using temporary directory ", data_dir)
      }
    } else {
      if (!quiet) message("Using existing user data directory: ", data_dir)
    }    
    the$data_dir <- data_dir
  }
  
  local_file <- file.path(data_dir, paste0(dataset_name, ".rda"))
  
  # 3. Check local user storage
  if (file.exists(local_file)) {
    if (!quiet) message("Loading dataset from user directory: ", local_file)
    e <- new.env()
    load(local_file, envir = e)
    if (exists(dataset_name, envir = e)) return(get(dataset_name, envir = e))
  }

  # 4. Try downloading from GitHub (silent failure)
  if (download) {
    gtihub_data <- NULL
    github_base <- "https://github.com/sigbertklinke/exams.forge/raw/main/inst/data-raw/"
    github_url  <- paste0(github_base, dataset_name, ".rda")
    #
    exists_remote <- tryCatch({
      res <- httr::HEAD(github_url)
      httr::status_code(res) == 200
    }, error = function(e) FALSE)
    
    if (exists_remote) {
      if (!quiet) message("Downloading dataset from GitHub: ", github_url)
      tmp_file    <- tempfile(fileext = ".rda")
      github_data <- tryCatch({
        suppressWarnings(download.file(github_url, tmp_file, quiet = TRUE))
        # File integrity check
        if (!file.exists(tmp_file) || file.info(tmp_file)$size == 0) stop("Download failed")
        e <- new.env()
        load(tmp_file, envir = e)
        if (exists(dataset_name, envir = e)) {
          if (!quiet) message("Downloaded dataset from GitHub: ", github_url)
          save(list = dataset_name, file = local_file, envir = e, compress = "xz")
          if (!quiet) message("Saved dataset to user directory: ", local_file)
          return(get(dataset_name, envir = e))
        } else stop("Object not found in .rda file")
      }, error = function(e) NULL)
      if (!is.null(github_data)) return(github_data)
    }
  }
  
  # 5. Compute dataset if needed
  if (!quiet) message("Computing dataset: ", dataset_name)
  #browser()
  computed_data <- sumofsquares(n, nmax, maxt = Inf, zerosum = TRUE)
  
  # Save computed dataset as sosN in user dir
  e <- new.env()
  assign(dataset_name, computed_data, envir = e)
  save(list = dataset_name, file = local_file, envir = e, compress = "xz")
  if (!quiet) message("Saved computed dataset to user directory: ", local_file)
  
  # Return computed dataset
  return(computed_data)
}
