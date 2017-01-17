#' @title Setup parallel processing, either multinode or multicore.
#'
#' @description
#' By default it uses a multinode cluster if available, otherwise sets up multicore via doMC.
#' Libraries required: parallel, doSNOW, doMC, RhpcBLASctl, foreach
#'
#' @param conf List whose "cores" element can be used to specify the number of
#'   multicore workers used.
#' @param type "any", "cluster"/"doSNOW", "doParallel", or "doMC"
#' @param allow_multinode If T will use multiple nodes if detected. If F will
#'   not use multiple machines even if they are available.
#' @param machine_list List of networked computers for multinode computation.
#' @param cpus_per_node Number of processes to run on each node, if using
#'   multinode parallelization.
#' @param outfile File to collect output across workers. IF "" then results are
#'   printed to the console.
#'
#' @return obj Cluster object that can be passed to stop_cluster().
#'
#' @seealso stop_cluster
#' @export
parallelize = function(conf = NULL, type="any", allow_multinode = T,
                       machine_list = Sys.getenv("SLURM_NODELIST"),
                       cpus_per_node = as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE")),
                       max_cores = NULL,
                       outfile = "" , verbose = F) {
  # Indicator for multi-node parallelism.
  multinode = F

  # Check if we have multiple machine access.
  if (allow_multinode) {
    machines = strsplit(machine_list, ",")[[1]]
    if (length(machines) > 1) {
      cat("Have multi-node access for parallelism with", length(machines), "machines:", machines, "\n")

      # Restrict the number of cores used, e.g. if we need a lot of memory per core.
      if (!is.null(max_cores)) {
        cpus_per_node = min(cpus_per_node, max_cores)
      }

      # NOTE: this may be a bad config if the nodes have different core counts.
      cores = rep(machines, each = cpus_per_node)
      cat("Multi-node cores enabled:", cores, "\n")
      multinode = T
    }
  }

  if (!multinode) {
    # Count of physical cores, unlike parallel::detectCores() which is logical cores (threads).
    cores = RhpcBLASctl::get_num_cores()
    cat("Local physical cores detected:", cores, "\n")

    # Restrict to max_cores if needed, e.g. high memory usage per core.
    if (!is.null(max_cores)) {
      if (max_cores < cores) {
        cat("Restricting usage to", max_cores, "cores.\n")
        cores = min(max_cores, cores)
      }
    }

    # TODO: remove this option.
    if (exists("conf") && !is.null(conf) && "num_cores" %in% names(conf)) {
      cores = conf$num_cores
      cat("Using", cores, "local processes due to conf settings.\n")
    }
  }

  if (multinode || type %in% c("cluster", "doSNOW")) {
    # Outfile = "" allows output from within foreach to be displayed while in RStudio.
    # TODO: figure out how to suppress the output from makeCluster()
    #capture.output({ cl = parallel::makeCluster(cores, outfile = outfile) })
    cat("Starting multinode cluster with cores:", cores, "\n")
    capture.output({ cl = snow::makeCluster(cores, type="SOCK", outfile = outfile) })
    # doParallel supports multicore and multinode parallelism, but may require
    # explicitly exporting functions and libraries across the cluster.
    doSNOW::registerDoSNOW(cl)
    parallel::setDefaultCluster(cl)
    parallel_type = "snow"
  } else if (type %in% c("doParallel") ||
             # doMC can't be used on Windows, so default to doParallel if doMC not installed.
             !"doMC" %in% rownames(installed.packages())) {
    # Outfile = "" allows output from within foreach to be displayed.
    # TODO: figure out how to suppress the output from makeCluster()
    capture.output({ cl = parallel::makeCluster(cores, outfile = outfile) })
    doParallel::registerDoParallel(cl)
    parallel::setDefaultCluster(cl)
    parallel_type = "multicore"
  } else {
    # doMC only supports multicore parallelism, not multi-node.
    doMC::registerDoMC(cores)
    cl = NA
    # TODO: is this needed since we've already done registerDoMC()?
    options(mc.cores = cores)
    parallel_type = "multicore"
  }

  # Make sure the BLAS is not competing with the SL parallelism.
  omp_threads = RhpcBLASctl::omp_get_max_threads()
  if (!is.null(omp_threads) && omp_threads > 1) {
    RhpcBLASctl::omp_set_num_threads(1)
  }

  # TODO: need to figure out difference between get_max_threads and get_num_procs.
  # They are not always both consistently set to 1 (i.e. on Benten).
  omp_threads = RhpcBLASctl::omp_get_max_threads()
  # If omp_threads is NULL we can safely plan on using 1 thread.
  omp_threads = ifelse(is.null(omp_threads), 1, omp_threads)
  cat("Our BLAS is setup for", RhpcBLASctl::blas_get_num_procs(), "threads and OMP is", omp_threads, "threads.\n")

  cat("doPar backend registered:", foreach::getDoParName(), "\n")
  cat("Workers enabled:", foreach::getDoParWorkers(), "\n")

  # Return invisibily so that NA is not printed.
  return(invisible(cl))
}
