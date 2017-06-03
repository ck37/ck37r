#' Setup a SuperLearner() based on parallel configuration.
#' @description Create custom SL and CV.SL functions that automatically use
#'   parallelization based on the provided configuration.
#' @param parallel Can be "multicore", "doSNOW"/"snow" or "seq"/NULL.
#' @param cluster Optional cluster, e.g. from SNOW.
#' @param outer_cv_folds How many folds to use for CV.SuperLearner
#' @param verbose If TRUE will output additional details during execution.
#' @return A list with a SL and CV.SL function.
#' @export
gen_superlearner = function(parallel = "multicore",
                            cluster = NULL,
                            outer_cv_folds = NULL,
                            verbose = T) {

  if (is.null(parallel) || parallel == "seq") {
    sl_fn = SuperLearner::SuperLearner
    # Treat cvControl as though it's referring to innerCvControl(),
    # due to new argument style in SuperLearner 2.0-22.
    cv_sl_fn = function(cvControl = list(), ...) {
      SuperLearner::CV.SuperLearner(...,
                                    V = outer_cv_folds,
                                    innerCvControl = list(cvControl))
      #SuperLearner::CV.SuperLearner(...)
    }
  } else if (parallel == "multicore") {
    if (verbose) {
      cat("Running SL via multicore\n")
    }
    sl_fn = function(...) {
      SuperLearner::mcSuperLearner(...)
    }
    #cv_sl_fn = function(...) {
    cv_sl_fn = function(cvControl = list(), ...) {
      SuperLearner::CV.SuperLearner(...,
                                    V = outer_cv_folds,
                                    innerCvControl = list(cvControl),
                                    parallel = parallel)
      #SuperLearner::CV.SuperLearner(..., parallel = parallel)
    }
  } else if (parallel %in% c("doSNOW", "snow")) {
    if (verbose) {
      cat("Running SL via snow\n")
    }
    sl_fn = function(...) {
      SuperLearner::snowSuperLearner(cluster, ...)
    }
    cv_sl_fn = function(cvControl = list(), ...) {
      SuperLearner::CV.SuperLearner(...,
                                    V = outer_cv_folds,
                                    innerCvControl = list(cvControl),
                                    parallel = cluster)
    }
  }
  results = list(sl_fn = sl_fn, cv_sl_fn = cv_sl_fn)
  results
}
