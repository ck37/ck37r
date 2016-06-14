#' @title Setup parallel processing, either multinode or multicore.
#'
#' @description
#' By default it uses a multinode cluster if available, otherwise sets up multicore via doMC.
#' Libraries required: parallel, doSNOW, doMC, RhpcBLASctl, foreach
#' @export
setup_parallelism = function(conf = NULL, type="either", allow_multinode = T,
                             machine_list = Sys.getenv("SLURM_NODELIST"),
                             cpus_per_node = as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE")),
                             outfile = "") {
  # Indicator for multi-node parallelism.
  multinode = F

  # Check if we have multiple machine access.
  if (allow_multinode) {
    machines = strsplit(machine_list, ",")[[1]]
    if (length(machines) > 1) {
      cat("Have multi-node access for parallelism with", length(machines), "machines:", machines, "\n")
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
    capture.output({ cl = parallel::makeCluster(cores, type="PSOCK", outfile = outfile) })
    # doParallel supports multicore and multinode parallelism, but may require
    # explicitly exporting functions and libraries across the cluster.
    registerDoSNOW(cl)
    setDefaultCluster(cl)
    parallel_type = "snow"
  } else if (type %in% c("doParallel")) {
    # Outfile = "" allows output from within foreach to be displayed.
    # TODO: figure out how to suppress the output from makeCluster()
    capture.output({ cl = parallel::makeCluster(cores, outfile = outfile) })
    registerDoParallel(cl)
    setDefaultCluster(cl)
  } else {
    # doMC only supports multicore parallelism, not multi-node.
    registerDoMC(cores)
    cl = NA
    # TODO: is this needed since we've already done registerDoMC()?
    options(mc.cores = cores)
    parallel_type = "multicore"
  }

  # Make sure the BLAS is not competing with the SL parallelism.
  omp_threads = omp_get_max_threads()
  if (!is.null(omp_threads) && omp_threads > 1) {
    omp_set_num_threads(1)
  }

  # TODO: need to figure out difference between get_max_threads and get_num_procs.
  # They are not always both consistently set to 1 (i.e. on Benten).
  omp_threads = omp_get_num_procs()
  # If omp_get_num_procs() returns NULL we can safely plan on using 1 thread.
  omp_threads = ifelse(is.null(omp_threads), 1, omp_threads)
  cat("Our BLAS is setup for", blas_get_num_procs(), "threads and OMP is", omp_threads, "threads.\n")
  #cat("Multicore parallel is setup to use", getOption("mc.cores"), "cores.\n")

  cat("doPar backend registered:", getDoParName(), "\n")
  cat("Workers enabled:", getDoParWorkers(), "\n")
  # Return invisibily so that NA is not printed.
  return(invisible(cl))
}

# Stop the cluster if parallel:makeCluster() was used, but nothing needed if doMC was used.
stop_cluster = function(cluster_obj) {
  # Check if this cluster was created using parallel:makeCluster
  if (inherits(cluster_obj, "cluster")) {
    stopCluster(cluster_obj)
  } else {
    #cat("No cluster shutdown required.\n")
    invisible()
  }
}
