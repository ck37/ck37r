#' @title Table of cross-validated Brier scores from CV.SuperLearner result
#' @description
#' Calculates cross-validated Brier score for each learner in the CV.SuperLearner.
#' Also calculates standard-error, confidence interval and p-value.
#'
#' @param x CV.SuperLearner object
#' @param y Outcome vector, if not already added to CV.SL object.
#' @param sort Sort table by order of score.
#' @param null_hypothesis Not implemented yet
#' @param two_tailed Not implemented yet
#' @param lower.tail Not implemented yet
#' @param ... Any additional unused arguments, due to the prauc_table generic.
#'
#' @return Dataframe table with Brier score and std dev.
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
#' brier_table(cvsl, y = y)
#'
# @references
#'
# @seealso \code{\link{cvsl_auc}} \code{\link{plot_roc.SuperLearner}}
#    \code{\link[cvAUC]{ci.cvAUC}}
#'
#' @importFrom stats pnorm
#'
# Need to explicitly document method due to period in the class name.
#' @method brier_table CV.SuperLearner
#'
#' @export
brier_table.CV.SuperLearner =
  function(x,
           y = x$Y,
           sort = TRUE,
           null_hypothesis = NULL,
           two_tailed = FALSE,
           lower.tail = TRUE,
           ...) {

  # Use a clearer object name.
  cvsl = x

  # Vector to save the fold id for each observation.
  fold_ids = rep(NA, length(cvsl$SL.predict))

  # Number of CV folds (or bootstrap repetitions in theory).
  n_samples = length(cvsl$folds)

  # Loop over each SL fold and extract which observations were in that fold.
  for (fold_i in seq(n_samples)) {
    fold_ids[cvsl$folds[[fold_i]]] = fold_i
  }

  # Dataframe to save score results.
  # Add 2 more than the # of learners to hold DiscreteSL + SL.
  result_df = data.frame(matrix(nrow = ncol(cvsl$library.predict) + 2, ncol = 4L))

  #colnames(aucs) = c("prauc", "se", "ci_lower", "ci_upper", "p-value")
  #colnames(result_df) = c("prauc", "sd")
  colnames(result_df) = c("brier", "stderr", "ci_lower", "ci_upper")#, "p-value")

  # Loop over each learner.
  for (learner_i in 1:nrow(result_df)) {
    # Create a default in case there is an error. Will be overwritten
    # if successful.
    #result = list(prauc = NA, se = NA, ci = c(NA, NA))
    result = list(brier = NA, sd = NA)

    try({
      if (learner_i <= ncol(cvsl$library.predict)) {

        result = brier_score(cvsl$library.predict[, learner_i], y, test_folds = fold_ids)

      } else if (learner_i == nrow(result_df)) {
        # Do SuperLearner after all of the learners + DiscreteSL.
        result = brier_score(cvsl$SL.predict, y, test_folds = fold_ids)
      } else {
        # Do discrete SL after all of the learners.
        result = brier_score(cvsl$discreteSL.predict, y, test_folds = fold_ids)
      }
    }, silent = TRUE)

    result_df[learner_i, "brier"] = result$brier
    #result_df[learner_i, "sd"] = result$sd

    std_err = result$sd / sqrt(n_samples)

    result_df[learner_i, "stderr"] = std_err

    ci = result$prauc + c(-1, 1) * 1.96 * std_err

    result_df[learner_i, "ci_lower"] = ci[1]
    result_df[learner_i, "ci_upper"] = ci[2]

    # Don't generate p-value yet.
  }

  rownames(result_df) = c(cvsl$libraryNames, "DiscreteSL", "SuperLearner")

  if (sort) {
    # Sort in descending order so best Brier scores are at bottom of table.
    result_df = result_df[order(result_df$brier, decreasing = TRUE), ]

  }

  return(result_df)
}
