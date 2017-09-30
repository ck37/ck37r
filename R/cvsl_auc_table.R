#' @title Deprecated, use auc_table() instead.
#' @description Deprecated, use auc_table() instead.
#'
#' @param cvsl CV.SuperLearner object
#' @param ... Remaining arguments that are passed through.
#'
#' @export
cvsl_auc_table = function(cvsl, ...) {
  .Deprecated("auc_table", "ck37r")

  # This will automatically pass through cvsl and any remaining arguments.
  UseMethod("auc_table")
}
