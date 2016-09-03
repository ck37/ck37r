#' Setup a SuperLearner() based on parallel configuration.
#' @return A list with a SL and CV.SL function.
gen_superlearner = function(parallel = "multicore",
                            cluster = NULL,
                            outer_cv_folds = NULL,
                            verbose = T) {

  if (is.null(parallel)) {
    sl_fn = SuperLearner::SuperLearner
    cv_sl_fn = function(...) {
      SuperLearner::CV.SuperLearner(..., V = outer_cv_folds)
    }
  } else if (parallel == "multicore") {
    if (verbose) {
      cat("Running SL via multicore\n")
    }
    sl_fn = function(...) {
      SuperLearner::mcSuperLearner(...)
    }
    cv_sl_fn = function(...) {
      SuperLearner::CV.SuperLearner(..., V = outer_cv_folds, parallel = parallel)
    }
  } else if (parallel == "snow") {
    if (verbose) {
      cat("Running SL via snow\n")
    }
    sl_fn = function(...) {
      SuperLearner::snowSuperLearner(cluster, ...)
    }
    cv_sl_fn = function(...) {
      SuperLearner::CV.SuperLearner(..., V = outer_cv_folds, parallel = cluster)
    }
  }
  results = list(sl_fn = sl_fn, cv_sl_fn = cv_sl_fn)
  results
}
