#' @title Calculate cross-validated AUC from CV.SuperLearner result
#' @description
#' Also calculates confidence interval.
#' Based on initial code by Alan Hubbard.
#' @references
#' Add Erin LeDell chapter reference.
#' @param cvsl CV.SuperLearner object
#' @return List with cvAUC and ci elements.
#' @importFrom cvAUC ci.cvAUC
#' @export
cvsl_auc = function(cvsl) {

  # Loop over each CV.SL fold and extract which observations were in that fold.
  fold_ids = rep(NA, length(cvsl$SL.predict))
  for (fold_i in 1:length(cvsl$fold)) {
    obs_index = unlist(cvsl$fold[fold_i])
    fold_ids[obs_index] = fold_i
  }

  # Create a default in case there is an error. Will be overwritten
  # if successful.
  result = list(cvAUC = 0, ci = c(0, 0))
  tryCatch({
    result = cvAUC::ci.cvAUC(cvsl$SL.predict, cvsl$Y, folds = fold_ids)
  }, error = function(e) {
    cat("Error in ci.cvAUC(), substituting default values.\n")
    print(e)
  })

  return(result)
}
