mrtool <- NULL

.onLoad <- function(libname, pkgname) {
  # library(reticulate)
  #install_miniconda()
  conda_create(envname = 'mrtool-0.1.0', python_version = '3.10')
  py_install(envname = 'mrtool-0.1.0', packages = c('mrtool==0.1.0', 'dill', 'spmat==0.0.9'), pip = TRUE)
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS='true');
  mrtool <<- reticulate::import("mrtool", delay_load = TRUE)
}
