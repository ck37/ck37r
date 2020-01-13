#' @title Table of PR-AUCs from SuperLearner result
#' @description
#' Calculates precision-recall AUC for each learner in the SuperLearner.
#'
#' @param x SuperLearner object
#' @param y Outcome vector, if not already added to SL object.
#' @param sort Sort table by order of AUC.
# @param null_hypothesis If NULL (default), use the highest observed AUC.
# @param two_tailed Two-failed null hypothesis test? Default FALSE.
# @param lower.tail Examine lower tail of test distribution? Default TRUE.
#' @param ... Any additional unused arguments, due to the prauc_table generic.
#'
#' @return Dataframe table with PR-AUCs.
#'
#' @examples
#' library(SuperLearner)
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' set.seed(1)
#' sl = SuperLearner(Boston$chas, subset(Boston, select = -chas),
#'                   family = binomial(),
#'                   SL.library = c("SL.mean", "SL.glm"))
#'
#' prauc_table(sl, y = Boston$chas)
#'
#' @references
#'
#' Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
#' Division of Biostatistics Working Paper Series. Paper 226.
#' http://biostats.bepress.com/ucbbiostat/paper266/
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#' http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
#
# TODO: add @seealso links.
#' @importFrom stats pnorm
#'
#' @export
prauc_table.SuperLearner = function(x, y = x$Y, sort = TRUE,
                                  #null_hypothesis = NULL,
                                  #two_tailed = FALSE,
                                  #lower.tail = TRUE,
                                  ...) {

  # Rename to a better object name.
  sl = x

  # Vector to save the fold id for each observation.
  fold_ids = rep(NA, length(sl$SL.predict))

  # Number of CV folds (or bootstrap repetitions in theory).
  n_samples = length(sl$validRows)

  # Loop over each SL fold and extract which observations were in that fold.
  for (fold_i in seq(n_samples)) {
    fold_ids[sl$validRows[[fold_i]]] = fold_i
  }

  # Dataframe to save PR-AUC and CI.
  #result_df = data.frame(matrix(nrow = ncol(sl$Z), ncol = 3L))
  result_df = data.frame(matrix(nrow = ncol(sl$Z), ncol = 5L))
  #colnames(result_df) = c("learner", "prauc", "sd")#, "ci_lower", "ci_upper", "p-value")
  colnames(result_df) = c("learner", "prauc", "stderr", "ci_lower", "ci_upper")#, "p-value")

  # Loop over each learner.
  for (learner_i in 1:ncol(sl$Z)) {
    # Create a default in case there is an error. Will be overwritten
    # if successful.
    result = list(prauc = NA, sd = NA, ci = c(NA, NA))
    try({
      result = prauc(sl$Z[, learner_i], y, test_folds = fold_ids)
    }, silent = TRUE)
    result_df[learner_i, "prauc"] = result$prauc

    std_err = result$sd / sqrt(n_samples)

    #result_df[learner_i, "sd"] = result$sd
    result_df[learner_i, "stderr"] = std_err

    ci = result$prauc + c(-1, 1) * 1.96 * std_err

    result_df[learner_i, "ci_lower"] = ci[1]
    result_df[learner_i, "ci_upper"] = ci[2]

  }

  # TODO: implement p-value calculation.


  result_df$learner = names(sl$cvRisk)

  if (sort) {
    # Sort in ascending order so best PR-AUCs are at bottom of table.
    result_df = result_df[order(result_df$prauc), ]

  }

  return(result_df)
}

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
