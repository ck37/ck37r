#' @title Deprecated, please use plot_roc() instead.
#' @description Deprecated, please use plot_roc() instead.
#'
#' @param cvsl CV.SuperLearner object
#' @param ... Remaining arguments that are passed through.
#'
#' @export
cvsl_plot_roc = function(cvsl, ...) {
  .Deprecated("plot_roc", "ck37r")

  # This will automatically pass through cvsl and any remaining arguments.
  UseMethod("plot_roc")
}
