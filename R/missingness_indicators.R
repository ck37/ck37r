#' @title Return matrix of missingness indicators for a dataframe or matrix.
#'
#' @description Return matrix of missingness indicators for a dataframe or
#'   matrix. Removes constant or collinear indicators.
#'
#' @param data Dataframe or matrix to analyze for missingness.
#' @param prefix Name prefix for new indicator columns.
#' @param remove_constant Remove any indicators that are all 0 or all 1.
#' @param remove_collinear Remove any indicators that are collinear with each other.
#' @param skip_vars Vector of variable names to skip.
#' @param verbose If TRUE, print additional information.
#'
#' @return Matrix of missingness indicators
#'
#' @examples
#'
#' # Load a test dataset.
#' data(PimaIndiansDiabetes2, package = "mlbench")
#'
#' # Check for missing values.
#' colSums(is.na(PimaIndiansDiabetes2))
#'
#' # Generate missingness indicators; skip outcome variable.
#' indicators = missingness_indicators(PimaIndiansDiabetes2, skip_vars = "diabetes")
#'
#' # Check missingness.
#' colSums(indicators)
#'
#' @seealso \code{\link{impute_missing_values}}
#'
#' @importFrom caret findLinearCombos
#'
#' @export
missingness_indicators = function(data, prefix = "miss_",
                                  remove_constant = T,
                                  remove_collinear = T,
                                  skip_vars = c(),
                                  verbose = F) {

  # Create indicators.
  indicators = sapply(data[, !colnames(data) %in% skip_vars], FUN = function(col) as.numeric(is.na(col)) )

  colnames(indicators) = paste0(prefix, colnames(data)[!colnames(data) %in% skip_vars])

  # Remove any indicators that are all 0 or all 1.
  if (remove_constant) {
    col_means = colMeans(indicators)
    if (verbose) {
      num_removed = sum(col_means %in% c(0, 1))
      if (num_removed > 0) {
        cat("Removing", num_removed, "indicators that are constant.\n")
      }
    }
    # Set drop = F to ensure we don't convert to a vector when
    # only one column remains.
    indicators = indicators[, !col_means %in% c(0, 1), drop = F]
  }

  # Remove any collinear indicators.
  if (remove_collinear) {

    # Use caret to identify collinearity.
    linear_combos = caret::findLinearCombos(indicators)

    remove_columns = linear_combos$remove

    if (length(linear_combos$remove) > 0) {
      if (verbose) {
        cat("Removing", length(linear_combos$remove), "indicators due to collinearity:\n")
        cat(paste0(colnames(indicators)[linear_combos$remove], collapse = ", "), "\n")
      }

      # Make sure we don't switch to a vector if only 1 column remains.
      indicators = indicators[, -linear_combos$remove, drop = F]
    }
  }

  return(indicators)
}
