#' @title Return matrix of missingness indicators for a dataframe or matrix.
#' @description Return matrix of missingness indicators for a dataframe or matrix.
#' @param data Dataframe or matrix to analyze for missingness.
#' @param prefix Name prefix for new indicator columns.
#' @param verbose If TRUE, print additional information.
#' @return Matrix of missingness indicators, with any constant indicators removed.
#' @export
missingness_indicators = function(data, prefix = "miss_",
                                  remove_constant = T, verbose = F) {
  # Create indicators.
  indicators = sapply(data, FUN = function(col) as.numeric(is.na(col)) )

  colnames(indicators) = paste0(prefix, colnames(data))

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

  return(indicators)
}