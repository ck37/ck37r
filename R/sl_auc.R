#' @title Table of cross-validated AUCs from SuperLearner result
#' @description
#' Calculates cross-validated AUC for each learner in the SuperLearner. Also
#' calculates standard-error, confidence interval and p-value. Based on initial
#' code by Alan Hubbard.
#'
#' @param sl CV.SuperLearner object
#' @param Y Outcome vector, if not already added to SL object.
#'
#' @return Dataframe table with auc, se, ci, and p-value (null hypothesis = 0.5).
#'
#' @examples
#' library(SuperLearner)
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' set.seed(1)
#' sl = SuperLearner(Boston$chas, subset(Boston, select = -chas), family = binomial(),
#'                  SL.library = c("SL.mean", "SL.glmnet"))
#'
#' sl_auc(sl, Y = Boston$chas)
#'
#' @references
#' LeDell, E., Petersen, M., & van der Laan, M. (2015). Computationally
#' efficient confidence intervals for cross-validated area under the ROC curve
#' estimates. Electronic journal of statistics, 9(1), 1583.
#'
#' @importFrom stats pnorm
#'
#' @export
sl_auc = function(sl, Y = sl$Y) {

  # Vector to save the fold id for each observation.
  fold_ids = rep(NA, length(sl$SL.predict))

  # Loop over each SL fold and extract which observations were in that fold.
  for (fold_i in 1:length(sl$validRows)) {
    fold_ids[sl$validRows[[fold_i]]] = fold_i
  }

  # Dataframe to save AUC and CI.
  aucs = data.frame(matrix(nrow = ncol(sl$Z), ncol = 5))
  colnames(aucs) = c("auc", "se", "ci_lower", "ci_upper", "p-value")

  # Loop over each learner.
  for (learner_i in 1:ncol(sl$Z)) {
    # Create a default in case there is an error. Will be overwritten
    # if successful.
    result = list(cvAUC = NA, se = NA, ci = c(NA, NA))
    try({
      result = cvAUC::ci.cvAUC(sl$Z[, learner_i], Y, folds = fold_ids)
    }, silent = T)
    aucs[learner_i, "auc"] = result$cvAUC
    aucs[learner_i, "se"] = result$se
    aucs[learner_i, "ci_lower"] = result$ci[1]
    aucs[learner_i, "ci_upper"] = result$ci[2]
    null_hypothesis = 0.5
    if (!is.na(result$cvAUC) && !is.na(result$se)) {
      # Asymptotically linear CI.
      pval = pnorm((result$cvAUC - null_hypothesis) / result$se, lower.tail = F)
    } else {
      pval = NA
    }
    aucs[learner_i, "p-value"] = pval
  }

  rownames(aucs) = names(sl$cvRisk)

  return(aucs)
}
