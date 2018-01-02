library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "MASS")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("Mode()")

data(Boston, package = "MASS")

table(Boston$chas)

Mode(Boston$chas)
