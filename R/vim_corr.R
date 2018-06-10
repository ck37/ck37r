#' Correlation analysis
#'
#' Examine variables most correlated with the outcome. Currently assumes that
#' outcome is binary variable.
#'
#' @param covariates Character vector listing the covariates to analyze.
#' @param data Dataframe of covariates and outcome.
#' @param outcome Outcome variable, assumed binary.
#' @param weights Optional observation weight vector.
#' @param bootse Whether bootstrap standard errors should be used for inference.
#' @param verbose If TRUE display extra information.
#' @export
#' @importFrom stats model.matrix na.omit
vim_corr =
  function(covariates, data, outcome,
           weights = rep(1, nrow(data)),
           bootse = TRUE,
           verbose = FALSE) {

  # TODO: parallelize via future.
  result = Reduce(rbind,
                  lapply(covariates, function(variable, data) {


                    results = data.frame(
                      "variable" = variable,
                      "corr" = NA,
                      "p_value" = NA,
                      "avg_con" = NA,
                      "avg_case" = NA,
                      note = "")

                    var_class = class(data[[variable]])

                    # Check if non-numeric.
                    # TODO: add test case for integer covariates.
                    # TODO: add test case for logical covariates.
                    if (!var_class %in% c("numeric", "integer")) {

                      results$note = paste("Class is", var_class)

                      # Stop early.
                      return(results)
                    }

                    # Check if all missing values.
                    if (sum(is.na(data[[variable]])) == nrow(data)) {
                      results$note = "Variable is all missing."

                      # Stop early.
                      return(results)
                    }

                    # Check if SD = 0.
                    if (length(unique(na.omit(data[[variable]]))) == 1L) {
                      results$note = paste("Variable has no variation")

                      # Stop early.
                      return(results)
                    }

                    # Raw mean difference.
                    means = tapply(data[[variable]], data[[outcome]], mean, na.rm = TRUE)

                    # TODO: check for cell size when stratifying by outcome.
                    # TODO: check for 0 standard deviation.

                    note = ""

                    # Correlation analysis.
                    #test = try(cor.test(data[[variable]], data[[outcome]],
                    #                    method = "pearson", na.action = na.omit), silent = TRUE)

                    # Include weights so that our rows are consistent.
                    temp_data = na.omit(data.frame(weights, data[, c(variable, outcome)]))
                    # Now extract those weights.
                    temp_weights = temp_data[[1]]
                    # And return to our original dataframe hopefully.
                    temp_data = temp_data[, -1]

                    # TODO: apply to a full matrix, not a single variable at a time.
                    #browser()
                    test = try(weights::wtd.cor(temp_data[[variable]], temp_data[[outcome]],
                                                weight = temp_weights, bootse = bootse), silent = TRUE)

                    if ("try-error" %in% class(test)) {
                      test = list(estimate = NA,
                                  p.value = NA)
                      if (verbose) {
                        note = "Correlation test failed"
                        cat(note, "for", variable, "\n")
                      }
                    } else {
                      test = list(estimate = test[1, "correlation"],
                                  p.value = test[1, "p.value"])
                    }


                    # outcome_values = unique(data[[outcome]])

                    results = data.frame("variable" = variable,
                                         "corr" = unname(test$estimate),
                                         #"corr" = test[1, "correlation"],
                                         "p_value" = test$p.value,
                                         #"p_value" = test[1, "p.value"],
                                         "avg_con" = means[1],
                                         "avg_case" = means[2],
                                         "note" = note)

                    return(results)
                  }, data = data))

  # Add FDR-adjusted p-value.
  result$p_value_fdr = stats::p.adjust(result$p_value, "BH")

  # Sort ascending by p-value, then negative abs correlation - which results
  # in largest abs correlations first (in case certain p-values tie, which does happen).
  result = result[order(result$p_value, -abs(result$corr)), ]
  result$rank = as.integer(rank(order(result$p_value, -abs(result$corr))))

  # Re-order columns.
  result = result[, c("rank", "variable", "corr", "p_value", "p_value_fdr", "avg_con", "avg_case", "note")]
  return(result)
}
