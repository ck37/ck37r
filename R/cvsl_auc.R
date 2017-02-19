#' @title Calculate cross-validated AUC from CV.SuperLearner result
#' @description
#' Also calculates confidence interval.
#' Based on initial code by Alan Hubbard.
#' @param cvsl CV.SuperLearner object
#'
#' @return List with cvAUC and ci elements.
#'
#' @examples
#'
#' library(SuperLearner)
#' library(ck37r)
#' data(Boston, package = "MASS")
#'
#' set.seed(1)
#' cvsl = CV.SuperLearner(Boston$chas, subset(Boston, select = -chas), family = binomial(),
#'                       cvControl = list(V = 2, stratifyCV = TRUE),
#'                       SL.library = c("SL.mean", "SL.glmnet"))
#' cvsl_auc(cvsl)
#'
#' @references
#' LeDell, E., Petersen, M., & van der Laan, M. (2015). Computationally
#' efficient confidence intervals for cross-validated area under the ROC curve
#' estimates. Electronic journal of statistics, 9(1), 1583.
#'
#' @importFrom cvAUC ci.cvAUC
#'
#' @seealso sl_auc cvAUC::ci.cvAUC
#'
#' @export
cvsl_auc = function(cvsl) {

  # Vector to save the fold id for each observation.
  fold_ids = rep(NA, length(cvsl$SL.predict))

  # Loop over each CV.SL fold and extract which observations were in that fold.
  for (fold_i in 1:length(cvsl$fold)) {
    obs_index = unlist(cvsl$fold[fold_i])
    fold_ids[obs_index] = fold_i
  }

  # Create a default in case there is an error. Will be overwritten
  # if successful.
  result = list(cvAUC = NA, se = NA, ci = c(NA, NA))
  tryCatch({
    result = cvAUC::ci.cvAUC(cvsl$SL.predict, cvsl$Y, folds = fold_ids)
  }, error = function(e) {
    cat("Error in ci.cvAUC(), substituting default values.\n")
    print(e)
  })

  return(result)
}
