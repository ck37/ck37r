#' @title Calculate cross-validated AUC from CV.SuperLearner result
#' @description
#' Also calculates confidence interval.
#' Based on initial code by Alan Hubbard.
#' @references
#' Add Erin LeDell chapter reference.
#' @param cvsl CV.SuperLearner object
#' @return List with cvAUC and ci elements.
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
