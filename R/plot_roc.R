# Create a generic function for usage, which other files will implement.
#' Plot the ROC curve for an ensemble object.
#'
#' Supports CV.SuperLearner and SuperLearner objects.
#' @param x Ensemble object.
#' @param ... Remaining arguments passed through to the method.
#' @export
# TODO: improve documentation.
plot_roc = function(x, ...) UseMethod("plot_roc")
