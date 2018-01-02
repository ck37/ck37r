library(ck37r)
library(SuperLearner)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "MASS")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

#################
# Setup data for testing.

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Subset rows to speed up example computation.
row_subset = sample(nrow(Boston), 200)

Boston = Boston[row_subset, ]

# Outcome needs to be a binary variable.
X = subset(Boston, select = -chas)

#################
# Sequential

sl = gen_superlearner(parallel = "seq", verbose = T)

# Test out sl and cvsl functions that were returned.
result = sl$sl_fn(Boston$chas, X[, 1:3], family = binomial(),
                  cvControl = list(V = 2, stratifyCV = T),
                  SL.library = c("SL.mean", "SL.glm"))
print(result)

# This will generate a warning about innerCvControl replication.
suppressWarnings({
  result = sl$cv_sl_fn(Boston$chas, X[, 1:3], family = binomial(),
                       cvControl = list(V = 2, stratifyCV = T),
                       SL.library = c("SL.mean", "SL.glm"))
})
print(summary(result))

#################
# Multicore

# NOTE: multicore needs to be skipped on windows systems.
if (Sys.info()[["sysname"]] != "Windows") {
  cluster = parallelize(type = "multicore", max_cores = 2, verbose = 2)

  sl = gen_superlearner(parallel = "multicore", verbose = T)

  # Test out sl and cvsl functions that were returned.
  result = sl$sl_fn(Boston$chas, X[, 1:3], family = binomial(),
                    cvControl = list(V = 2, stratifyCV = T),
                    SL.library = c("SL.mean", "SL.glm"))
  print(result)


  # This will generate a warning about innerCvControl replication.
  suppressWarnings({
    result = sl$cv_sl_fn(Boston$chas, X[, 1:3], family = binomial(),
                       cvControl = list(V = 2, stratifyCV = T),
                       SL.library = c("SL.mean", "SL.glm"))
  })
  print(summary(result))

  stop_cluster(cluster, verbose = T)
}

#################
# Snow

if (F) {
# Limit to 2 cores for CRAN testing.
  # This causes devtools::check() to hang, need to investigate.
cluster = parallelize(type = "doSNOW", max_cores = 2)


# Make sure each node loads SuperLearner so that All() screener can be found.
invisible(clusterEvalQ(cluster, library(SuperLearner)))

sl = gen_superlearner(parallel = "snow", cluster = cluster, verbose = T)

  # Test out sl and cvsl functions that were returned.
  result = sl$sl_fn(Boston$chas, X[, 1:3], family = binomial(),
                    cvControl = list(V = 2, stratifyCV = T),
                    SL.library = c("SL.mean", "SL.glm"))
  print(result)

  # This will generate a warning about innerCvControl replication.
  suppressWarnings({
    # Note: this needs a modified version of SuperLearner where
    # the bug in grabbing the method in snowSuperLearner is fixed.
    result = sl$cv_sl_fn(Boston$chas, X[, 1:3], family = binomial(),
                         cvControl = list(V = 2, stratifyCV = T),
                         SL.library = c("SL.mean", "SL.glm"))
  })
  print(summary(result))

  stop_cluster(cluster, verbose = T)
}
