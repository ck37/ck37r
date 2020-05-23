#' Calculate Brier score from cross-validation results
#'
#' @param preds Predicted values or score
#' @param actual Known label value
#' @param test_folds Length n vector assigning each vector to a given test fold
#'
#' @return Dataframe of results, including Brier score and standard deviation
brier_score = function(preds, actual, test_folds) {

  df = na.omit(data.frame(preds, actual, test_folds))

  # Loop over each test fold
  # Calculate the Brier score
  # Take the average

  unique_folds = unique(test_folds)

  result = sapply(unique_folds, function(test_fold_i) {
    test_df = subset(df, test_folds == test_fold_i)

    brier_score = mean((test_df$preds - test_df$actual)^2)

    brier_score
  })

  # num_folds = length(unique_folds)

  brier = mean(result)

  result = list(brier = brier,
                sd = sd(result),
                ci = c(NA, NA))

  result
}
