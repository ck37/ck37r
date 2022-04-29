#' @title Table of RMSEs from SuperLearner result
#' @description
#' Calculates root mean-squared error and CIs for each learner in the SuperLearner.
#'
#' @param x SuperLearner object
#' @param y Outcome vector, if not already added to SL object.
#' @param sort Sort table by order of AUC.
# @param null_hypothesis If NULL (default), use the highest observed AUC.
# @param two_tailed Two-failed null hypothesis test? Default FALSE.
# @param lower.tail Examine lower tail of test distribution? Default TRUE.
#' @param version 1 (default) or 2. 1 averages the RMSE over folds; 2 averages
#' the MSE and then takes the square root.
#' @param ... Any additional unused arguments, due to the prauc_table generic.
#'
#' @return Dataframe table with RMSEs.
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
#' rmse_table(sl, y = Boston$chas)
#'
#' @references
#'
#' Faber, N. K. M. (1999). Estimating the uncertainty in estimates of root mean
#' square error of prediction: application to determining the size of an
#' adequate test set in multivariate calibration. Chemometrics and Intelligent
#' Laboratory Systems, 49(1), 79-89.
#'
#' Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
#' Division of Biostatistics Working Paper Series. Paper 226.
#' http://biostats.bepress.com/ucbbiostat/paper266/
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#' http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
#'
#
# TODO: add @seealso links.
#' @importFrom stats pnorm
#'
#' @export
rmse_table.SuperLearner = function(x, y = x$Y, sort = TRUE,
                                   version = 1,
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

  # Dataframe to save RMSE and CI.
  result_df = data.frame(matrix(nrow = ncol(sl$Z), ncol = 5L))

  colnames(result_df) = c("learner", "rmse", "stderr", "ci_lower", "ci_upper")#, "p-value")

  # Loop over each learner.
  for (learner_i in 1:ncol(sl$Z)) {
    # Create a default in case there is an error. Will be overwritten
    # if successful.
    result = list(rmse = NA, std_err = NA, ci = c(NA, NA))
    try({
      result = rmse(sl$Z[, learner_i], y, test_folds = fold_ids, version = version)
    }, silent = TRUE)
    result_df[learner_i, "rmse"] = result$rmse

    result_df[learner_i, "stderr"] = result$std_err

    result_df[learner_i, "ci_lower"] = result$ci[1]
    result_df[learner_i, "ci_upper"] = result$ci[2]

  }

  # TODO: implement p-value calculation.


  result_df$learner = names(sl$cvRisk)

  if (sort) {
    # Sort in descending order so best RMSEs are at bottom of table.
    result_df = result_df[order(result_df$rmse, decreasing = TRUE), ]

  }

  return(result_df)
}

