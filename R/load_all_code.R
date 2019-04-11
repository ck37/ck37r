#' Load all R files in a library directory.
#'
#' @param lib_dir Directory contains the source code files.
#' @param exclude_files Exclude a list of files; exclude function_library.R by
#'   default because we presume that is the main R library file.
#' @param file_pattern Regular expression for files to load, defaults to *.R
#' @param recursive If TRUE also recurse into subdirectories. Defaults to TRUE.
#' @param verbose If TRUE display additional output during execution.
#' @param envir Environment in which to run the code; .GlobalEnv by default,
#'  but baseenv() is a good option to force package references.
#'
#' @examples
#'
#' library(ck37r)
#'
#' # Here R is a subdirectory with a bunch of .R files to load.
#' load_all_code("R")
#'
#' @seealso sys.source, source
#'
#' @importFrom crayon red bold
#'
#' @export
load_all_code = function(lib_dir = "lib",
                         exclude_files = c("function_library.R"),
                         file_pattern = "\\.R$",
                         recursive = T,
                         verbose = T,
                         envir = .GlobalEnv) {
  # Load all .R files in the lib directory.
  lib_files =  list.files(path = lib_dir, file_pattern, full.names = F,
                          recursive = recursive)

  # Exclude any files that we don't need to load.
  lib_files = setdiff(lib_files, exclude_files)

  # Error highlighting in console output.
  error = red $ bold

  # Loop over file list and load each file.
  for (file in lib_files) {
    file_name = paste0(lib_dir, "/", file)
    if (verbose) {
      cat("Sourcing", file_name, "\n")
    }
    #source(file_name)

    # Run code in a tryCatch() so that we can continue loading other files
    # even if a subset generates errors.
    tryCatch({
        sys.source(file_name, envir = envir)
      },
      error = function(e) {
        cat(error("Error"), "sourcing", crayon::bold(file), "\n")
        print(e)
        cat("\n")
      })

  }
}
