#' Calculate IPA measure from cross-validation results
#'
#' @param preds Predicted values or score
#' @param actual Known label value
#' @param test_folds Length n vector assigning each vector to a given test fold
#'
#' @return Dataframe of results, including Brier score and standard deviation
ipa_measure = function(preds, actual, test_folds) {

  df = na.omit(data.frame(preds, actual, test_folds))

  # Loop over each test fold
  # Calculate the Brier score
  # Take the average

  unique_folds = unique(test_folds)

  result = sapply(unique_folds, function(test_fold_i) {
    test_df = subset(df, test_folds == test_fold_i)

    brier_score = mean((test_df$preds - test_df$actual)^2)

    # Brier score of the null model (just take outcome mean).
    null_model = mean((mean(test_df$actual) - test_df$actual)^2)

    ipa = 1 - brier_score / null_model

    ipa

  })

  # num_folds = length(unique_folds)

  ipa = mean(result)

  result = list(ipa = ipa,
                sd = sd(result),
                ci = c(NA, NA))

  result
}
