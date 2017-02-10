#' @title Setup TMLE to run in parallel
#'
#' @description
#' Starts a cluster and sets up TMLE and SuperLearner to use the cluster
#' so that TMLE is conducted in parallel rather than using only one core.
#'
#' @param parallel "multicore", "doParallel", or "doSNOW"
#' @param max_cores Restrict how many many cores will be used on a machine,
#'   rather than using all available cores. Useful if each core needs to use a
#'   substantial amount of memory.
#' @param allow_multinode If T, will create a multinode cluster if it finds
#'   multiple machines listed in the "SLURM_NODELIST" environmental variable.
#'   If F, it will only use the current node even if multiple nodes are detected.
#' @param global If TRUE will create run_tmle() as a function in the global
#'   environment and "cl" as a global cluster object.
#' @export
#' @seealso parallelize, tmle_parallel, gen_superlearner
# TODO: add examples to the code, document return object.
setup_parallel_tmle = function(parallel = "multicore", max_cores = NULL,
                               allow_multinode = T, global = T) {

  # Start cluster.
  cl = ck37r::parallelize(type = parallel, max_cores = max_cores,
                            allow_multinode = allow_multinode)

  # Create SuperLearner function.
  sl_functions = ck37r::gen_superlearner(parallel = parallel, cluster = cl)

  # Create a run_tmle function that automatically uses this parallel setup.
  run_tmle = function(...) {
    ck37r::tmle_parallel(..., sl_fn = sl_functions$sl_fn)
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