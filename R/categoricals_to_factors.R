#' @title Convert categorical numerics to factors
#'
#' @description Convert categorical numerics to factors
#'
#' @param data Dataframe
#' @param categoricals Vector of column names that are categoricals.
#' @param verbose If T display extra output during execution.
#'
#' @return Updated dataframe.
#'
#' @export
categoricals_to_factors = function(data, categoricals,
                                   verbose = F) {
  for (var_name in categoricals) {
    if (!var_name %in% colnames(data)) {
      if (verbose) {
        cat("Skipping", var_name, "- was not in the data frame.\n")
      }
      next
    }

    # Use [[]] to support tibbles.
    if (class(data[[var_name]]) == "factor") {
      if (verbose) {
        cat("Skipping", var_name, "- already a factor.\n")
      }
      next
    }

    if (verbose) {
      cat("Converting", var_name, "from", class(data[[var_name]]), "to factor.")
      cat(" Unique vals:", length(unique(data[[var_name]])), "\n")
    }

    # Use [[]] to support tibbles.
    data[[var_name]] = as.factor(data[[var_name]])
  }

  return(data)
}
