#' Starts an h2o cluster on multiple nodes
#'
#' Can be used on a SLURM system or any cluster where h2o can take advantage
#' of multiple computers.
#'
#' @param h2o_jar Path to h2o.jar, e.g. "~/software/h2o-latest/h2o.jar"
#' @param node_list Comma-separated list of nodes to use, defaults to
#' the value of SLURM_NODELIST. Assumed to be hostnames, not IPs.
#' @param memory_per_node Memory per node. Default 8g.
#' @param cluster_name Name of the h2o cluster. Default "h2o_r".
#' @param verbose If T display extra information during execution.
#' @param ... Remaining arguments passed to h2o.init()
#'
#' @examples
#' \dontrun{
#' cl = h2o_init_multinode("~/software/h2o-latest/h2o.jar")
#'
#' h2o.shutdown()
#' }
#'
#' @export
h2o_init_multinode =
  function(h2o_jar,
           node_list = Sys.getenv("SLURM_NODELIST"),
           memory_per_node = "8g",
           cluster_name = "h2o_r",
           verbose = T, ...) {
  if (verbose) {
    cat("SLURM nodes:", node_list, "\n")
  }

  # Loop up IPs of the allocated nodes.
  if (node_list != "") {
    nodes = strsplit(node_list, ",")[[1]]
    ips = rep(NA, length(nodes))
    for (i in 1:length(nodes)) {
      args = c("hosts", nodes[i])
      # TODO: skip this step if the nodelist is already IPs.
      result = system2("getent", args = args, stdout = T)
      # Extract the IP from the result output.
      ips[i] = sub("^([^ ]+) +.*$", "\\1", result, perl = T)
    }
    if (verbose) {
      cat("Node IPs:", paste(ips, collapse=", "), "\n")
    }
    # Combine into a network string for h2o.
    network = paste0(paste0(ips, "/32"), collapse=",")
    if (verbose) {
      cat("Network:", network, "\n")
    }
  }

  # Specify how many nodes we want h2o to use.
  h2o_num_nodes = length(ips)

  # Options to pass to java call:
  args = c(
    # -Xmx30g allocate 30GB of RAM per node. Needs to come before "-jar"
    paste0("-Xmx", memory_per_node),
    # Specify path to downloaded h2o jar.
    paste("-jar", h2o_jar),
    # Specify a name for the cluster.
    paste("-name ", cluster_name),
    # Specify IPs of other nodes.
    paste("-network", network)
  )
  if (verbose) {
    cat(paste0("Args:\n", paste(args, collapse = "\n"), "\n"))
  }

  # Run once for each node we want to start.
  for (node_i in 1:h2o_num_nodes) {
    if (verbose) {
      cat("\nLaunching h2o worker on", ips[node_i], "\n")
    }
    new_args = c(ips[node_i], "java", args)
    # Ssh into the target IP and launch an h2o worker with its own
    # output and error files. These could go in a subdirectory.
    cmd_result = system2("ssh", args = new_args,
                         stdout = paste0("h2o_out_", node_i, ".txt"),
                         stderr = paste0("h2o_err_", node_i, ".txt"),
                         # Need to specify wait=F so that it runs in the background.
                         wait = F)
    # This should be 0.
    if (verbose) {
      cat("Cmd result:", cmd_result, "\n")
    }
    # Wait one second between inits.
    Sys.sleep(1L)
  }

  # Wait 3 more seconds to find all the nodes, otherwise we may only
  # find the node on localhost.
  Sys.sleep(3L)

  # Check if h2o is running. We will see ssh processes and one java process.
  system2("ps", c("-ef", "| grep h2o.jar"), stdout = T)

  # Connect to our existing h2o cluster.
  # Do not try to start a new server from R.
  h2o::h2o.init(startH2O = F, ...)
}
