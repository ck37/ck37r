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
#' set.seed(1, "L'Ecuyer-CMRG")
#'
#' # Subset rows to speed up example computation.
#' row_subset = sample(nrow(Boston), 100)
#'
#' Boston = Boston[row_subset, ]
#' X = subset(Boston, select = -chas)
#'
#' cvsl = CV.SuperLearner(Boston$chas, X[, 1:2], family = binomial(),
#'                       cvControl = list(V = 2, stratifyCV = TRUE),
#'                       SL.library = c("SL.mean", "SL.glm"))
#' cvsl_auc(cvsl)
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
#' @seealso \code{\link{auc_table.CV.SuperLearner}} \code{\link{plot_roc.CV.SuperLearner}}
#'   \code{\link[cvAUC]{ci.cvAUC}}
#'
#' @importFrom cvAUC ci.cvAUC
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
