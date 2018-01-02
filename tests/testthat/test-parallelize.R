library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

# Set to only 2 cores per CRAN policies.
max_cores = 2

###########
# Defaults
if (F) {
cl = parallelize(max_cores = max_cores)

# No effect for doMC but shuts down cluster for doSNOW or Windows-doParallel.
stop_cluster(cl)
}

###########
# doSNOW
if (F) {
cl = parallelize("doSNOW", max_cores = max_cores)

stop_cluster(cl)
}

###########
# doParallel.
if (F) {
cl = parallelize("doParallel", max_cores = max_cores)

stop_cluster(cl)
}

###########
# Sequential

cl = parallelize("seq")

cl = parallelize(NULL)

###########
# Multinode
if (F) {
cl = parallelize("doSNOW", max_cores = max_cores,
                 machine_list = rep("localhost", 2), cpus_per_node = 1)

stop_cluster(cl)
}
