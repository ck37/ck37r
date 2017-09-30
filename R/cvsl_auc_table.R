#' @title Deprecated wrapper to auc_table.CV.SuperLearner()
#' @description Deprecated, please use

#' @param cvsl CV.SuperLearner object
#'
#' @return Dataframe table with auc, se, ci, and p-value.
#'
#' @export
cvsl_auc_table = function(cvsl, ...) {
  .Deprecated("auc_table", "ck37r")

  # This will automatically pass through cvsl and any remaining arguments.
  UseMethod("auc_table")
}
