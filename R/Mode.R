#' @title Compute the mode of a vector (can be multiple results).
#'
#' @description
#'
#' Via http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#'
#' @param x vector
#'
#' @return Vector of modal values in arbitrary order.
#'
#' @examples
#'
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' table(Boston$chas)
#'
#' Mode(Boston$chas)
#'
#' @export
Mode = function(x) {
  ux = unique(x)
  tab = tabulate(match(x, ux))
  # This will return multiple modes if they exist.
  ux[tab == max(tab)]
}
