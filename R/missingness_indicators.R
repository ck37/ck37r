#' @export
missingness_indicators = function(data, prefix = "miss_") {
  # Create indicators.
  indicators = sapply(data, FUN = function(col) as.numeric(is.na(col)) )
  colnames(indicators) = paste0(prefix, colnames(data))
  col_means = colMeans(indicators)
  # Remove indicators that are all 0 or all 1.
  indicators = indicators[, !col_means %in% c(0, 1)]
  indicators
}
