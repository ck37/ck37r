# Create a generic function for usage, which other files will implement.
#' @export
auc_table = function(x, ...) UseMethod("auc_table")
