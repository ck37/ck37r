#' Return amount of RAM allocated to rJava
#'
#' This presumes that the rJava package has already been loaded/initialized.
#'
#' @param verbose If TRUE display more detailed information during execution.
#'
#' @examples
#'
#' get_java_memory()
#'
#' get_java_memory(verbose = TRUE)
#'
#' @export
get_java_memory = function(verbose = FALSE) {
  # Confirm that rJava is installed.
  if (!requireNamespace("rJava", quietly = TRUE)) {
    cat("rJava not installed, doing nothing.\n")
    return(NA)
  }

  if (rJava:::.need.init()) {
    # Java has not yet been initialized.
    return(NA)
  }

  java_env = tryCatch(rJava::.jnew("java/lang/Runtime", check = FALSE, silent = TRUE))

  found_memory = rJava::.jcall(java_env, "J", "maxMemory")

  if (verbose) {
    cat("RAM allocated to rJava:", round((found_memory / 1e9), 2), "\n")
  }

  return(found_memory)
}
