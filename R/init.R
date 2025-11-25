#' Load options
#' @export
.onLoad <- function(libname, pkgname){
  if (is.null(getOption("DHRtools.loglevel"))){
    options(DHRtools.loglevel = "all")
  }
} 