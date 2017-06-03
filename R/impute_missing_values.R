#' Impute missing values in a dataframe and add missingness indicators.
#'
#' @description Impute missing values, using knn by default or alternatively
#'   median-impute numerics, mode-impute factors. Add missingness indicators.
#'
#' @param data Dataframe or matrix.
#' @param type "knn" or "standard" (median/mode). NOTE: knn will result in the
#'   data being centered and scaled!
#' @param add_indicators Add a series of missingness indicators.
#' @param prefix String to add at the beginning of the name of each missingness
#'   indicator.
#' @param skip_vars List of variable names to exclude from the imputation.
#' @param verbose If True display extra information during execution.
#'
#'
#' @return List with the following elements:
#' \itemize{
#' \item $data - imputed dataset.
#' \item $impute_info - if knn, caret preprocess element for imputing test data.
#' \item $impute_values - if standard, list of imputation values for each variable.
#' }
#'
#' @examples
#'
#' # Load a test dataset.
#' data(PimaIndiansDiabetes2, package = "mlbench")
#'
#' # Check for missing values.
#' colSums(is.na(PimaIndiansDiabetes2))
#'
#' # Impute missing data and add missingness indicators.
#' # Don't impute the outcome though.
#' result = impute_missing_values(PimaIndiansDiabetes2, skip_vars = "diabetes")
#'
#' # Confirm we have no missing data.
#' colSums(is.na(result$data))
#'
#'
#' #############
#' # K-nearest neighbors imputation
#'
#' result2 = impute_missing_values(PimaIndiansDiabetes2, type = "knn", skip_vars = "diabetes")
#'
#' # Confirm we have no missing data.
#' colSums(is.na(result2$data))
#'
#' @seealso \code{\link{missingness_indicators}} \code{\link[caret]{preProcess}}
#'
#' @importFrom stats median
#' @importFrom RANN nn2
#'
#' @export
impute_missing_values = function(data,
                                 type = "standard",
                                 add_indicators = T,
                                 prefix = "miss_",
                                 skip_vars = c(),
                                 verbose = F) {
  # Loop over each feature.
  missing_indicators = NULL

  # Make a copy to store the imputed dataframe.
  new_data = data

  # List of results to populate.
  # Save our configuration first.
  results = list(type = type,
                 add_indicators = add_indicators,
                 skip_vars = skip_vars,
                 prefix = prefix)

  if (type == "standard") {
    if (verbose) {
      cat("Running standard imputation.\n")
    }
    preprocess = NA

    # List to save the imputation values used.
    # We need a list because it can contain numerics and factors.
    impute_values = vector("list", ncol(data))

    # Copy variable names into the imputed values vector.
    names(impute_values) = colnames(data)

    # TODO: vectorize, and support parallelization.
    for (i in 1:ncol(data)) {
      # Use double brackets rather than [, i] to support tibbles.
      nas = sum(is.na(data[[i]]))
      # cat("Processing", colnames(data)[i], "class:", class(data[[i]]), "\n")

      col_class = class(data[[i]])
      if (col_class == "factor") {
        # Impute factors to the mode.
        # Choose the first mode in case of ties.
        impute_value = Mode(data[[i]])[1]
      } else if (col_class %in% c("integer", "numeric")) {
        # Impute numeric values to the median.
        impute_value = median(data[[i]], na.rm = T)
      } else {
        warning(paste(colnames(data)[i],
                      "should be numeric or factor type. But its class is",
                      col_class))
      }

      # Save the imputed value even if there is no missingness.
      # We may need this for future data.
      impute_values[[i]] = impute_value

      # Nothing to impute, continue to next column.
      # TODO: add note and also skip if nas are 100% of the data.
      if (nas == 0 || names(data)[i] %in% skip_vars) {
        next
      } else if (nas == nrow(data)) {
        if (verbose) {
          cat("Note: skipping", colnames(data)[i], "because all values are NA.\n")
        }
        next
      } else {
        # Make the imputation.
        new_data[is.na(data[[i]]), i] = impute_value
      }

    }

    results$impute_values = impute_values

  } else if (type == "knn") {
    impute_info = caret::preProcess(new_data, method = c("knnImpute"))
    new_data = predict(impute_info, new_data)
    results$impute_info = impute_info

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

  results$data = new_data

  results

}
