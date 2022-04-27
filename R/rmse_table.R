# Create a generic function for usage, which other files will implement.
#' Generate a table of RMSEs by learner
#'
#' Supports SuperLearner objects currently.
#'
#' @param x SuperLearner object
#' @param ... Remaining arguments passed through to the methods.
#' @export
# TODO: improve generic documentation.
rmse_table = function(x, ...) UseMethod("rmse_table")
