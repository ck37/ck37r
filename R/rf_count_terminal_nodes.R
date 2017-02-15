#' Count the terminal nodes in each tree from a random forest
#' @param rf Random Forest object
#' @return vector of terminal node counts
#' @examples
#'
# TODO: flesh out example
# terminal_nodes = rf_count_terminal_nodes(rf)
# summary(terminal_nodes)
#' @export
rf_count_terminal_nodes = function(rf) {
  terminal_nodes = rep(NA, rf$forest$ntree)

  # TODO: vectorize
  for (tree_i in 1:rf$forest$ntree) {
    # Extract a single tree from the forest.
    tree = randomForest::getTree(rf, tree_i, labelVar = F)

    # Terminal nodes have NA as their split variable.
    sum_na = sum(is.na(tree$`split var`))

    terminal_nodes[tree_i] = sum_na
  }

  return(terminal_nodes)
}
