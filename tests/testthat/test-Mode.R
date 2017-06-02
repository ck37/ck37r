library(ck37r)
library(testthat)

context("Mode()")

data(Boston, package = "MASS")

table(Boston$chas)

Mode(Boston$chas)
