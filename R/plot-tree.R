#' Plot rpart decision tree with pretty good defaults.
#'
#' @param tree tbd
#' @param shadow.col tbd
#' @param shadow.offset tbd
#' @param box.palette tbd
#' @param digits tbd
#' @param pal.thresh tbd
#' @param extra tbd
#' @param tweak tbd
#' @param ... Additional arguments passed through to prp()
#'
# TODO: document arguments, add examples & related methods.
#' @export
plot_tree =
  function(tree,
           shadow.col = "gray",
           shadow.offset = 0.2,
           box.palette = "auto",
           digits = 3,
           pal.thresh = 0.04,
           extra = 107,
           # Adjust text size, 1.2 = 20% larger than the automatic choice.
           tweak = 1.2,
           ...) {
  rpart.plot::prp(tree,
      shadow.col = shadow.col,
      shadow.offset = shadow.offset,
      box.palette = box.palette,
      digits = digits,
      pal.thresh = pal.thresh,
      #extra = 101,
      extra = extra,
      tweak = tweak,
      ...)
}
