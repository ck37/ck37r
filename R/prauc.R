#' Calculate PR-AUC from cross-validation results
#'
#' Uses the precrec package to calculate PR-AUC
#'
#' @param preds Predicted values or score
#' @param actual Known label value
#' @param test_folds Length n vector assigning each vector to a given test fold
#'
#' @return Dataframe of results, including PR-AUC and standard deviation
prauc = function(preds, actual, test_folds) {

  df = na.omit(data.frame(preds, actual, test_folds))

  # Loop over each test fold
  # Calculate the PR-AUC
  # Take the average

  unique_folds = unique(test_folds)

  result = sapply(unique_folds, function(test_fold_i) {
    test_df = subset(df, test_folds == test_fold_i)

    sscurves = precrec::evalmod(scores = test_df$preds, labels = test_df$actual)

    # Extract the PR-AUC
    prauc = attr(sscurves$prcs[[1]], "auc")

    prauc
  })

  # num_folds = length(unique_folds)

  prauc = mean(result)

  result = list(prauc = prauc,
                sd = sd(result),
                ci = c(NA, NA))

  result
}
