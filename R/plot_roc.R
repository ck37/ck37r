# Create a generic function for usage, which other files will implement.
#' @export
plot_roc = function(x, ...) UseMethod("plot_roc")
