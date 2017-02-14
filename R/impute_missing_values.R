#' Impute missing values in a dataframe and add missingness indicators.
#' @description Median-impute numerics, Mode-impute factors, add missingness indicators.
#' @param data Dataframe or matrix.
#' @param add_indicators Add a series of missingness indicators.
#' @param prefix String to add at the beginning of the name of each missingness
#'   indicator.
#' @param skip_vars List of variable names to exclude from the imputation.
#' @param verbose If True display extra information during execution.
#' @importFrom stats median
#'
#' @export
impute_missing_values = function(data, add_indicators = T, prefix = "miss_",
                                 skip_vars = c(), verbose = F) {
  # Loop over each feature.
  missing_indicators = NULL

  # Make a copy to store the imputed dataframe.
  new_data = data

  # TODO: vectorize, and support parallelization.
  for (i in 1:ncol(data)) {
    # Use double brackets rather than [, i] to support tibbles.
    nas = sum(is.na(data[[i]]))
    # Nothing to impute, continue to next column.
    # TODO: add note and also skip if nas are 100% of the data.
    if (nas == 0 || names(data)[i] %in% skip_vars) {
      next
    } else if (nas == nrow(data)) {
      if (verbose) {
        cat("Note: skipping", colnames(data)[i], "because all values are NA.\n")
      }
      next
    }

    if (class(data[[i]]) == "factor") {
      # Impute factors to the mode.
      # Choose the first mode in case of ties.
      new_data[is.na(data[[i]]), i] = Mode(data[[i]])[1]
    } else {
      # Impute numeric values to the median.
      new_data[is.na(data[[i]]), i] = median(data[[i]], na.rm = T)
    }
  }
  if (add_indicators) {
    # Create missingness indicators from original dataframe.
    missing_indicators = missingness_indicators(data, prefix = prefix, verbose = verbose)

    if (verbose) {
      cat("Indicators added:", ncol(missing_indicators), "\n")
    }

    # Append indicators.
    new_data = cbind(new_data, missing_indicators)
  }
  new_data
}
