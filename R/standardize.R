#' @title Rescale variables, possibly excluding some columns
#'
#' @description Simple extension to base::scale() to skip columns.
#'
#' @param x Dataframe or matrix, assumed to have column names.
#' @param skip_vars List of names of variables not to scale.
#' @param ... Extra arguments passed-through to base::scale
#'
#' @return Data-frame with appropriate variables scaled.
#'
#' @examples
#'
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' # Don't scale our outcome variable.
#' data = standardize(Boston, skip_vars = "medv")
#'
#' summary(data)
#'
#' @seealso \code{\link[base]{scale}}
#'
#' @export
standardize = function(x, skip_vars = NULL, ...) {
  original_order = colnames(x)
  new_df = cbind(data.frame(scale(x[, !colnames(x) %in% skip_vars, drop = FALSE], ...)),
             x[, colnames(x) %in% skip_vars, drop = FALSE])
  # Return with columns in their original order, rather than re-ordered
  # due to the data.frame() call above.
  new_df[, original_order]
}
