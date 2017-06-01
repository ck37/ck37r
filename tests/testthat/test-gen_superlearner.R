library(ck37r)
library(testthat)

#################
# Multicore
sl = gen_superlearner(parallel = "multicore", verbose = T)

# TODO: test out sl and cvsl elements.

#################
# Sequential
sl = gen_superlearner(parallel = "seq", verbose = T)

# TODO: test out sl and cvsl elements.

#################
# Snow

sl = gen_superlearner(parallel = "snow", verbose = T)

# TODO: test out sl and cvsl elements.