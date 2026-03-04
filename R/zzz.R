#' @import rjson tinytex yaml

the <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname){
  the$opening <- c("("="\\left(", "["="\\left[", "{"="\\left\\{", "|"="|", "||"="\\|", 
                      "<"="\\langle", "a"="\\langle", "c"="\\lceil", "f"="\\lfloor")
  the$closing <- c("("="\\right)", "["="\\right]", "{"="\\right\\}{", "|"="|", "||"="\\|", 
                      "<"="\\rangle", "a"="\\rangle", "c"="\\rceil", "f"="\\rfloor")
  # load distributions
  data("distributions", envir=the, package="exams.forge")
#  source(system.file("dist", "craft.R", package="exams.forge"), local=TRUE)
#  utils::data("distributions", package=pkgname, envir=parent.env(environment()))
  stranslate::loadMsg(system.file("msg", "messages.txt", package="exams.forge"), .domain="exams.forge", .overwrite=TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0('exams.forge ', utils::packageVersion("exams.forge"), 
                               ': see the package vignette with `vignette("exams.forge")`'))
}
