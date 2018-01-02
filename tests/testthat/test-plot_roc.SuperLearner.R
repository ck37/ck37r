library(SuperLearner)
library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "MASS")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

data(Boston, package = "MASS")

set.seed(1)
sl = SuperLearner(Boston$chas, subset(Boston, select = -chas),
                  family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.glm"))

sl

plot_roc(sl, y = Boston$chas)

# Deprecated version, will generate a warning.
suppressWarnings({
  sl_plot_roc(sl, y = Boston$chas)
})
