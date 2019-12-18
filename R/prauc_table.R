# Create a generic function for usage, which other files will implement.
#' Generate a table of PR-AUCs by learner
#'
#' Currently supports only SuperLearner objects.
#'
#' @param x SuperLearner object
#' @param ... Remaining arguments passed through to the methods.
#' @export
# TODO: improve generic documentation.
prauc_table = function(x, ...) UseMethod("prauc_table")
