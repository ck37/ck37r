# Create a generic function for usage, which other files will implement.
#' Generate a table of Brier scores by learner
#'
#' Supports CV.SuperLearner objects currently.
#'
#' @param x CV.SuperLearner object
#' @param ... Remaining arguments passed through to the methods.
#' @export
# TODO: improve generic documentation.
brier_table = function(x, ...) UseMethod("brier_table")
