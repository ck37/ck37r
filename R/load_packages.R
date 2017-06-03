#' Load a list of packages.
#'
#' Load packages and install them from CRAN if they aren't already available.
#'
#' @param pkgs Character vector of packages to load.
#' @param auto_install Install any packages that could not be loaded.
#' @param update Update packages where possible.
#' @param verbose If T display more detailed information during execution.
#' @param ... Any additional parameters to pass through to install.packages()
#'
#' @importFrom utils capture.output install.packages update.packages
#'
#' @examples
#'
#' # Load these 4 packages and install them if necessary.
#' load_packages(c("MASS", "SuperLearner", "tmle", "doParallel"), auto_install = TRUE)
#'
#' @export
load_packages = function(pkgs = NULL, auto_install = F, update = F,
                         verbose = F, ...) {

  # Attempt an update first, in case it will help with installing new packages.
  update_result = NULL
  if (update) {
    # Update any R packages that can be updated.
    update_result = update.packages(ask = F, checkBuilt = T)
  }

  # Try to load each package, and save whether or not it succeeded.
  capture.output({ result = sapply(pkgs, require, character.only=T, quietly=T) })

  install_result = NULL
  result_retry = NULL

  # Return a helpful message and the install.packages command if needed.
  if (sum(!result) > 0) {
    cat("\n\nThese packages need to be installed:", paste(pkgs[!result], collapse=", "), "\n")
    install_code = paste0('install.packages(c("', paste(pkgs[!result], collapse='", "'), '"))')
    cat(install_code, "\n")

    if (auto_install) {
      cat("Auto-installing from repository:", getOption("repos")[1], "\n")
      install_result = install.packages(pkgs[!result], ...)
      # Try to load newly installed packages.
      capture.output({ result_retry = sapply(pkgs[!result], require, character.only=T, quietly=T) })
    }
  } else {
    install_code = ""
  }


  results = list(packages = pkgs, pkgs_result = result, pkgs_retry = result_retry,
                 install_code = install_code, update_result = update_result)
  invisible(result)
}
