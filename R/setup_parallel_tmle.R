#' @title Setup TMLE to run in  parallel
#' @export
setup_parallel_tmle = function(parallel = "multicore", allow_multinode = T, global = T) {

  # Start cluster.
  cl = ckTools::parallelize(type = parallel, allow_multinode = allow_multinode)

  # Create SuperLearner function.
  sl_functions = ckTools::gen_superlearner(parallel = parallel, cluster = cl)

  # Create a run_tmle function that automatically uses this parallel setup.
  run_tmle = function(...) {
    ckTools::tmle_parallel(..., sl_fn = sl_functions$sl_fn)
  }

  # Save cl and run_tmle as global variables for easier usage.
  if (global) {
    assign("cl", cl, envir = .GlobalEnv)
    assign("run_tmle", run_tmle, envir = .GlobalEnv)
  }

  # Compile results into a list.
  results = list(
    cluster = cl,
    run_tmle = run_tmle,
    sl_functions = sl_functions
  )

  return(invisible(results))
}
