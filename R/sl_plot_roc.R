#' @title Deprecated, please use plot_roc() instead.
#' @description Deprecated, please use plot_roc() instead.
#'
#' @param sl SuperLearner object
#' @param ... Remaining arguments that are passed through.
#'
#' @export
sl_plot_roc = function(sl, ...) {
  .Deprecated("plot_roc", "ck37r")

  # This will automatically pass through sl and any remaining arguments.
  UseMethod("plot_roc")
}
