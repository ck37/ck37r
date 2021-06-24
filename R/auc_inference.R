#' Estimate AUC and 95 CI
#'
#' @param true Vector of true values, 0 or 1
#' @param pred Vector of predicted values used to sort the data
#' @param alpha Type I error rate of the statistical test
#' @export
# TODO: support observation weights
auc_inference =
  function(true,
           pred,
           alpha = 0.05) {
  auc = PRROC::roc.curve(pred, weights.class0 = true)$auc
  # For WeightedROC, outcome must be -1 or 1
  #auc = WeightedROC::WeightedAUC(WeightedROC::WeightedROC(prediction, outcome))
  #auc = pROC::auc(prediction, outcome)

  ####
  # Calculate standard error.
  # Count number of positive and negative cases.
  # Perhaps these could be weight sums for observation weights.
  num_neg = sum(true == 0)
  num_pos = sum(true)
  se_auc = auctestr::se_auc(auc, num_pos, num_neg)

  ci = auc + c(-1, 1) * qnorm(1 - alpha / 2) * se_auc

  results = list(
    auc = auc,
    se = se_auc,
    ci = ci)

  results
}
