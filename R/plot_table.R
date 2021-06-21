if (getRversion() >= "2.15.1")  utils::globalVariables(c("learner", "ci_lower", "ci_upper"))
#' Plot a table of values using ggplot
#' @param x Table of ML results, assuming to have "learner", "ci_lower", and "ci_upper" columns.
#' @param metric Column to plot
#' @param sort If TRUE (default), sort rows based on the metric.
#' @importFrom ggplot2 ggplot geom_pointrange aes coord_flip labs theme_minimal
#' @export
plot_table = function(x,
                      metric = "auc",
                      sort = TRUE) {
  # Use a clearer object name.
  tab = x
  if (!is.null(sort)) {
    tab = tab[order(tab[[metric]], decreasing = sort), ]
  }
  # Convert to a factor with manual levels so ggplot doesn't re-order
  # alphabetically.
  tab$learner = factor(tab$learner, levels = tab$learner)
  rownames(tab) = NULL
  p =
    ggplot2::ggplot(tab,
                    ggplot2::aes(x = learner, y = base::get(metric),
                                ymin = ci_lower, ymax = ci_upper)) +
    ggplot2::geom_pointrange(fatten = 2) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Learner", y = metric) + ggplot2::theme_minimal()
  return(p)
}
