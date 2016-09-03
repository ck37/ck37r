#' Impute missing values in a dataframe and add missingness indicators.
#'
#' @param data Dataframe or matrix.
#' @param add_indicators Add a series of missingness indicators.
#' @param prefix String to add at the beginning of the name of each missingness
#'   indicator.
#' @param skip_vars List of variable names to exclude from the imputation.
#'
#' @export
impute_missing_values = function(data, add_indicators = T, prefix = "miss_",
                                 skip_vars = c()) {
  # Loop over each feature.
  missing_indicators = NULL
  for (i in 1:ncol(data)){
    nas = sum(is.na(data[, i]))
    # Nothing to impute, continue to next column.
    # TODO: add note and also skip if nas are 100% of the data.
    if (nas == 0 || names(data)[i] %in% skip_vars) {
      next
    }

    missing_indicator = matrix(1 * is.na(data[, i]))
    new_name = paste0(prefix, colnames(data)[i])
    colnames(missing_indicator) = new_name
    next_col = ncol(missing_indicators) + 1
    if (is.null(missing_indicators)) {
      missing_indicators = matrix(missing_indicator, nrow=nrow(data), ncol=1)
      colnames(missing_indicators) = new_name
    } else {
      missing_indicators = cbind(missing_indicators, missing_indicator)
    }

    if (class(data[, i]) == "factor") {
      # Impute factors to the mode.
      # Choose the first mode in case of ties.
      data[is.na(data[, i]), i] = Mode(data[, i])[1]
    } else {
      # Impute numeric values to the median.
      data[is.na(data[, i]), i] = median(data[, i], na.rm=T)
    }
  }
  if (add_indicators) {
    data = cbind(data, missing_indicators)
  }
  data
}
