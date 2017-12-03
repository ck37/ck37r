library(testthat)
library(ck37r)

context("java_memory")

# With verbose set.
set_java_memory("4g", verbose = TRUE)

# Default version.
set_java_memory("2g")

# No memory specified.
set_java_memory()

get_java_memory(verbose = TRUE)

get_java_memory()
