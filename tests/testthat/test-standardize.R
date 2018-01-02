library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "MASS")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("Standardize")

data(Boston, package = "MASS")

# Don't scale our outcome variable.
data = standardize(Boston, skip_vars = "medv")

summary(data)
