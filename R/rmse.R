#' Calculate RMSE from cross-validation results
#'
#' @param preds Predicted values or score
#' @param actual Known value
#' @param test_folds Length n vector assigning each vector to a given test fold
#' @param version 1 (default) or 2. 1 averages the RMSE over folds; 2 averages
#' the MSE and then takes the square root.
#'
#' @return Dataframe of results, including RMSE, standard error, and CI
#'
#' @references
#' Faber, N. K. M. (1999). Estimating the uncertainty in estimates of root mean
#' square error of prediction: application to determining the size of an
#' adequate test set in multivariate calibration. Chemometrics and Intelligent
#' Laboratory Systems, 49(1), 79-89.
#'
rmse = function(preds, actual, test_folds, version = 1) {

  df = na.omit(data.frame(preds, actual, test_folds))

  unique_folds = unique(test_folds)

  results = lapply(unique_folds, function(test_fold_i) {
    test_df = subset(df, test_folds == test_fold_i)

    mse = mean((test_df$preds - test_df$actual)^2)
    rmse = sqrt(mse)

    n = nrow(test_df)

    # Calculate standard error via Faber (1999) equation 10.
    #std_err = 1 / (sqrt(2) * n * rmse) * sqrt(...)
    # Equation 5:
    std_err = rmse * sqrt(1 / (2 * n))

    result = list(rmse = rmse,
                  mse = mse,
                  std_err = std_err,
                  var = std_err^2)
    result
  })

  # num_folds = length(unique_folds)

  df_results = do.call(rbind, lapply(results, data.frame))

  if (version == 1) {
    rmse = mean(df_results$rmse)
  } else {
    # Version 2.
    # We are averaging the MSEs and then taking the sqrt.
    # This may be closer to the SL built-in MSE estimates.
    rmse = sqrt(mean(df_results$mse))
  }

  # We are averaging the variances and then taking the sqrt.
  std_err = sqrt(mean(df_results$var))

  ci = c(rmse - std_err * qnorm(0.975),
         rmse + std_err * qnorm(0.975))

  result = list(rmse = rmse,
                std_err = std_err,
                ci = ci)

  result
}
