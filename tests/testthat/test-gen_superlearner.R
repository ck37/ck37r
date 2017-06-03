library(ck37r)
library(testthat)

#################
# Multicore
if (F) {
sl = gen_superlearner(parallel = "multicore", verbose = T)
}

# TODO: test out sl and cvsl elements.
# NOTE: multicore needs to be skipped on windows systems.

#################
# Sequential
sl = gen_superlearner(parallel = "seq", verbose = T)

# TODO: test out sl and cvsl elements.

#################
# Snow
sl = gen_superlearner(parallel = "snow", verbose = T)

# TODO: test out sl and cvsl elements.
