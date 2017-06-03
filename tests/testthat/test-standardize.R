library(ck37r)
library(testthat)

context("Standardize")

data(Boston, package = "MASS")

# Don't scale our outcome variable.
data = standardize(Boston, skip_vars = "medv")

summary(data)
