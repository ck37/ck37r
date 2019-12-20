#' @title Table of cross-validated AUCs from CV.SuperLearner result
#' @description
#' Calculates cross-validated AUC for each learner in the CV.SuperLearner. Also
#' calculates standard-error, confidence interval and p-value. Based on initial
#' code by Alan Hubbard.
#'
#' @param x CV.SuperLearner object
#' @param y Outcome vector, if not already added to CV.SL object.
#' @param sort Sort table by order of AUC.
#' @param null_hypothesis Default is the highest AUC from the learners.
#' @param two_tailed Two-failed null hypothesis test? Default FALSE.
#' @param lower.tail Examine only lower tail of test distribution? Default FALSE.
#' @param ... Any additional unused arguments, due to the auc_table generic.
#'
#' @return Dataframe table with auc, se, ci, and p-value (null hypothesis = 0.5)
#'
#' @examples
#' library(SuperLearner)
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' set.seed(1)
#' y = as.numeric(Boston$medv > 23)
#' cvsl = CV.SuperLearner(Y = y,
#'                        X = subset(Boston, select = -medv),
#'                        family = binomial(),
#'                        cvControl = list(V = 2, stratifyCV = TRUE),
#'                        SL.library = c("SL.mean", "SL.glm"))
#' auc_table(cvsl, y = y)
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
#' @seealso \code{\link{cvsl_auc}} \code{\link{plot_roc.SuperLearner}}
#'    \code{\link[cvAUC]{ci.cvAUC}}
#'
#' @importFrom stats pnorm
#'
# Need to explicitly document method due to period in the class name.
#' @method auc_table CV.SuperLearner
#'
#' @export
auc_table.CV.SuperLearner = function(x, y = x$Y, sort = TRUE,
                                     null_hypothesis = NULL,
                                     two_tailed = FALSE,
                                     lower.tail = TRUE,
                                     ...) {

  # Use a clearer object name.
  cvsl = x

  # Vector to save the fold id for each observation.
  fold_ids = rep(NA, length(cvsl$SL.predict))

  # Loop over each SL fold and extract which observations were in that fold.
  for (fold_i in 1:length(cvsl$folds)) {
    fold_ids[cvsl$folds[[fold_i]]] = fold_i
  }

  # Dataframe to save AUC and CI.
  # Add 2 more than the # of learners to hold DiscreteSL + SL.
  aucs = data.frame(matrix(nrow = ncol(cvsl$library.predict) + 2, ncol = 5))
  colnames(aucs) = c("auc", "se", "ci_lower", "ci_upper", "p-value")

  # Loop over each learner.
  for (learner_i in 1:nrow(aucs)) {
    # Create a default in case there is an error. Will be overwritten
    # if successful.
    result = list(cvAUC = NA, se = NA, ci = c(NA, NA))

    try({
      if (learner_i <= ncol(cvsl$library.predict)) {
        result = cvAUC::ci.cvAUC(cvsl$library.predict[, learner_i], y, folds = fold_ids)
      } else if (learner_i == nrow(aucs)) {
        # Do SuperLearner after all of the learners + DiscreteSL.
        result = cvAUC::ci.cvAUC(cvsl$SL.predict, y, folds = fold_ids)
      } else {
        # Do discrete SL after all of the learners.
        result = cvAUC::ci.cvAUC(cvsl$discreteSL.predict, y, folds = fold_ids)
      }
    }, silent = TRUE)

    aucs[learner_i, "auc"] = result$cvAUC
    aucs[learner_i, "se"] = result$se
    aucs[learner_i, "ci_lower"] = result$ci[1]
    aucs[learner_i, "ci_upper"] = result$ci[2]
    # Don't generate p-value yet.
  }

  if (is.null(null_hypothesis)) {
    # Use the highest observed AUC as the null hypothesis.
    null_hypothesis = max(aucs$auc, na.rm = TRUE)
  }

  # Loop one more time, this time calculating the auc p-value.
  for (learner_i in 1:nrow(aucs)) {
    # Specify drop = F so that we have a 1-row dataframe and can use $.
    result = aucs[learner_i, , drop = F]
    if (!is.na(result$auc) && !is.na(result$se)) {
      # Asymptotically linear CI.

      # If two_tailed = TRUE multiply by 2, otherwise multiply by 1.
      pval = (as.numeric(two_tailed) + 1) *
        pnorm((result$auc - null_hypothesis) / result$se,
              lower.tail = lower.tail)
    } else {
      pval = NA
    }
    aucs[learner_i, "p-value"] = pval
  }

  rownames(aucs) = c(cvsl$libraryNames, "DiscreteSL", "SuperLearner")

  if (sort) {
    # Sort in ascending order so best AUCs are at bottom of table.
    # This will also place NAs at the bottom of the table.
    aucs = aucs[order(aucs$auc), ]

  }

  return(aucs)
}
