library(testthat)
try(detach(package:ck37r), silent = T)
library(ck37r)

context("Test tmle_parallel()")

# Use Causal Inference's lab 5 data as test data.
file = system.file("extdata", "RLab5.TMLE.csv", package = "ck37r")

data = read.csv(file)
str(data)

W = subset(data, select = -c(Y, A))

# Use multiple cores as available.
# CRAN check requires at most 2 cores.
parallel = setup_parallel_tmle(max_cores = 2)

# Basic SL library.
sl_lib = c("SL.mean", "SL.glm")

library(SuperLearner)

# Set a parallel-compatible seed so cross-validation folds are deterministic.
set.seed(1, "L'Ecuyer-CMRG")
result = run_tmle(Y = data$Y, A = data$A, W = W, family = "binomial",
                  g.SL.library = sl_lib, Q.SL.library = sl_lib,
                  conserve_memory = F, verbose = T)
result
result$time
print(object.size(result), units = "MB")

# If we remove the Q and g objects we get back to 0.5 MB.
result2 = result
result2$sl_Q = NULL
result2$sl_g = NULL
print(object.size(result2), units = "MB")

# What if we just remove the fit libraries?
result3 = result
result3$sl_Q$fitLibrary = NULL
result3$sl_g$fitLibrary = NULL
# Down to 2.6 MB - partly there.
print(object.size(result3), units = "MB")

# Compare to normal tmle
set.seed(1, "L'Ecuyer-CMRG")
tmle = tmle::tmle(Y = data$Y, A = data$A, W = W, family = "binomial",
                  g.SL.library = sl_lib, Q.SL.library = sl_lib)
tmle

print(object_size(tmle))
print(object.size(tmle), units = "MB")

# Compare to conserve_memory version.
set.seed(1, "L'Ecuyer-CMRG")
result_cm = run_tmle(Y = data$Y, A = data$A, W = W, family = "binomial",
                  g.SL.library = sl_lib, Q.SL.library = sl_lib,
                  conserve_memory = T, verbose = T)
result_cm
result_cm$time
# 1.3 MB - not too shabby.
print(object.size(result_cm), units = "MB")
result_cm$sl_Q$cvRisk

# Dig into the sl memory usage details.
obj = result$sl_Q

# $fitLibrary is what uses up most of the memory.
# Smaller contributors: SL.predict, library.predict, Z
for (name in names(obj)) {
  elm = obj[[name]]
  cat("Element:", name, "Type:", pryr::sexp_type(elm), "Size:")
  try(print(pryr::object_size(elm)))
  try(print(utils::object.size(elm), units = "MB"))
}
