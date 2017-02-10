#' @title Rescale variables, possibly excluding some columns
#' @description Simple extension to base::scale() to skip columns.
#' @param x Dataframe or matrix, assumed to have column names.
#' @param ... Extra arguments passed-through to base::scale
#' @param exclude_vars List of names of variables not to scale.
#' @return Data-frame with appropriate variables scaled.
#' @export
standardize = function(x, ..., exclude_vars = NULL) {
  original_order = colnames(x)
  new_df = data.frame(scale(x[, !colnames(x) %in% exclude_vars], ...),
             x[, colnames(x) %in% exclude_vars])
  # Return with columns in their original order, rather than re-ordered
  # due to the data.frame() call above.
  new_df[, original_order]
}
