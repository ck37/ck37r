#' Import all CSV files in a given directory and save them to a list.
#' @param directory Directory to search for files.
#' @param file_pattern File pattern to match.
#' @param recursive Whether or not recurse into subdirectories, default T.
#' @param verbose If True display additional information during execution.
#'
#' @return List with files; filenames are the names of the list elements (with extension removed).
#'
#' @examples
#'
#' library(ck37r)
#'
#' files = import_csvs("extdata")
#'
#' names(files)
#'
#' @export
import_csvs = function(directory = "", file_pattern = "\\.csv$",
                       recursive = T, verbose = T) {

  file_names = list.files(path=directory, file_pattern, full.names = F,
                          recursive = recursive)
  if (verbose) {
    cat(paste0("Found ", length(file_names), " text files in \"", directory, "\" to import.\n"))
  }

  if (length(file_names) == 0) {
    warning(paste("did not find any files to load."))
    return(list())
  }

  time_start = proc.time()

  files = list()
  for (file in file_names) {
    # Remove the file extension from the file.
    list_name = stringr::str_to_lower(gsub(file_pattern, "", file))
    # Import the csv file.
    data = reader::reader(file, directory, header=T, def = ",")
    # Lowercase the column names.
    colnames(data) = sapply(colnames(data), FUN=stringr::str_to_lower)
    files[[list_name]] = data
  }

  time_end = proc.time()

  # Double-check how many files we loaded.
  if (verbose) {
    cat("Time to import files:\n")
    print(time_end - time_start)
    cat("Total files imported:", length(files), "\n")
  }

  # Return the result.
  return(files)
}
