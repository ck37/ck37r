library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("java_memory")

# With verbose set.
set_java_memory("4g", verbose = TRUE)

# Default version.
set_java_memory("2g")

# No memory specified.
set_java_memory()

get_java_memory(verbose = TRUE)

get_java_memory()
