library(ck37r)
library(testthat)

context("test_load_packages")

# Load these 4 packages but don't install any.
load_packages(c("MASS", "SuperLearner", "tmle", "doParallel"))

# Load packages and update installed packages.
# load_packages(c("MASS"), update = T)

# Load these 4 packages and install them if necessary.
load_packages(c("MASS", "SuperLearner", "tmle", "doParallel"), auto_install = T)

# Try to install a non-existent package.
# This will generate warnings.
suppressWarnings(try({
  load_packages("ck37_blah123", auto_install = T)
}))
