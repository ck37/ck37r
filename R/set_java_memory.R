#' Allocate memory to rJava
#'
#' This function should be run before rJava is loaded.
#'
#' @param memory Amount of memory to allocate to rJava, e.g. "4g".
#' @param verbose If TRUE display more detailed output during execution.
#'
#' @importFrom rJava .jcall .jnew
#' @examples
#'
#' # Set java maximum memory usage to 4 GB.
#' set_java_memory("4g", verbose = TRUE)
#'
#' @export
set_java_memory = function(memory = NULL, verbose = FALSE) {
  # TODO: check if rJava is already loaded, in which case this is too late.
  if (!rJava:::.need.init()) {
    warning("rJava has already been loaded, allocated memory cannot be modified.")
  }

  if (!is.null(memory)) {
    if (verbose) {
      cat("Allocating RAM to rJava:", memory, "\n")
    }
    options(java.parameters = paste0("-Xmx", memory))
  }

}
