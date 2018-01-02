library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("test_load_packages")

# Load these packages but don't install any.
load_packages(c("SuperLearner", "tmle", "doParallel"))

# Load packages and update installed packages.
# Doesn't work on travis-ci or appyveyor currently :/
# load_packages(c("MASS"), update = T)

# Load these packages and install them if necessary.
load_packages(c("SuperLearner", "tmle", "doParallel"), auto_install = TRUE)

# Try to install a non-existent package.
# This will generate warnings.
suppressWarnings(try({
  load_packages("ck37_blah123", auto_install = TRUE)
}))
