#' @title Deprecated, use auc_table() instead.
#' @description Deprecated, use auc_table() instead.
#'
#' @param sl SuperLearner object
#' @param ... Remaining arguments that are passed through.
#'
#' @export
sl_auc_table = function(sl, ...) {
  .Deprecated("auc_table", "ck37r")
  UseMethod("auc_table")
}
