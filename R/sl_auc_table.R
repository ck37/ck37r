#' @title Table of cross-validated AUCs from SuperLearner result
#' @description
#' Calculates cross-validated AUC for each learner in the SuperLearner. Also
#' calculates standard-error, confidence interval and p-value. Based on initial
#' code by Alan Hubbard.
#'
#' @param sl CV.SuperLearner object
#' @param Y Outcome vector, if not already added to SL object.
#' @param sort Sort table by order of AUC.
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
#' sl_auc_table(sl, Y = Boston$chas)
#'
#' @references
#' LeDell, E., Petersen, M., & van der Laan, M. (2015). Computationally
#' efficient confidence intervals for cross-validated area under the ROC curve
#' estimates. Electronic journal of statistics, 9(1), 1583.
#'
#' Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
#' Division of Biostatistics Working Paper Series. Paper 226.
#' http://biostats.bepress.com/ucbbiostat/paper266/
#'
#' Sing, T., Sander, O., Beerenwinkel, N., & Lengauer, T. (2005). ROCR:
#' visualizing classifier performance in R. Bioinformatics, 21(20), 3940-3941.
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#' http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
#'
#' @seealso \code{\link{cvsl_auc}} \code{\link{sl_plot_roc}} \code{\link[cvAUC]{ci.cvAUC}}
#'
#' @importFrom stats pnorm
#'
#' @export
sl_auc_table = function(sl, Y = sl$Y, sort = T) {

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

  if (sort) {
    # Sort in ascending order so best AUCs are at bottom of table.
    aucs = aucs[order(aucs$auc), ]

  }

  return(aucs)
}
