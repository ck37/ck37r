# Create a generic function for usage, which other files will implement.
#' Generate a table of IPA scores by learner
#'
#' IPA = index of prediction accuracy (Kattan & Gerds 2018)
#'
#' Supports CV.SuperLearner objects currently.
#'
#' @param x CV.SuperLearner object
#' @param ... Remaining arguments passed through to the methods.
#' @export
# TODO: improve generic documentation.
ipa_table = function(x, ...) UseMethod("ipa_table")
