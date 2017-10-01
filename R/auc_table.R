# Create a generic function for usage, which other files will implement.
#' Generate a table of AUCs by learner
#'
#' Supports both CV.SuperLearner and SuperLearner objects.
#'
#' @param x SuperLearner or CV.SuperLearner object
#' @param ... Remaining arguments passed through to the methods.
#' @export
# TODO: improve generic documentation.
auc_table = function(x, ...) UseMethod("auc_table")
