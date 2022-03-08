#' Calculate PR-AUC from cross-validation results
#'
#' Uses the precrec package to calculate PR-AUC
#'
#' @param preds Predicted values or score
#' @param actual Known label value
#' @param test_folds Length n vector assigning each vector to a given test fold
#'
#' @return Dataframe of results, including PR-AUC and standard deviation
#'
#' @references
#'
#' Boyd, K., Eng, K. H., & Page, C. D. (2013). Area under the precision-recall
#' curve: point estimates and confidence intervals. In Joint European conference
#' on machine learning and knowledge discovery in databases (pp. 451-466).
#' Springer, Berlin, Heidelberg.
#'
prauc = function(preds, actual, test_folds) {

  df = na.omit(data.frame(preds, actual, test_folds))


  # Determine which is less common, the 1s or the 0s.
  # We suspect the 1s but in some datasets it may be the 0s.
  freq_tab = table(actual)

  least_common_result = freq_tab[unname(which.min(freq_tab)[1])]
  least_common_label = as.numeric(names(least_common_result))

  # Loop over each test fold
  # Calculate the PR-AUC
  # Take the average

  unique_folds = unique(test_folds)

  results = lapply(unique_folds, function(test_fold_i) {
    test_df = subset(df, test_folds == test_fold_i)

    sscurves = precrec::evalmod(scores = test_df$preds, labels = test_df$actual)

    # Extract the PR-AUC
    prauc = attr(sscurves$prcs[[1]], "auc")


    # Calculate squared standard error based on normal approximation of the binomial,
    # as shown in Boyd et al. (2013).
    # prauc * (1 - prauc) / (# of least-common labels)
    effective_sample_size = sum(test_df$actual == least_common_label)
    var = prauc * (1 - prauc) / effective_sample_size

    # Std error of PR-AUC on the logit scale, for use in CIs.
    # See Boyd et al. (2013).
    tau_hat = 1 / sqrt(effective_sample_size * prauc * (1 - prauc))

    result = list(prauc = prauc,
                  var = var,
                  tau_hat = tau_hat)
    result
  })

  # num_folds = length(unique_folds)

  df_results = do.call(rbind, lapply(results, data.frame))

  prauc = mean(df_results$prauc)
  std_err = sqrt(mean(df_results$var))

  # Calculate CI via Boyd et al. (2013).
  eta_hat = log(prauc / (1 - prauc))
  # We square the logit-based standard errors, take the mean, the sqrt to aggregate.
  tau_hat = sqrt(mean(df_results$tau_hat^2))
  # Almost exactly the same results if we simply average over the folds.
  #tau_hat = mean(df_results$tau_hat)

  # See Boyd et al. (2013). Logit intervals.
  ci = c(plogis(eta_hat - tau_hat * qnorm(0.975)),
         plogis(eta_hat + tau_hat * qnorm(0.975)))

  result = list(prauc = prauc,
                std_err = std_err,
                ci = ci)

  result
}
