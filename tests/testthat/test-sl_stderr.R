library(SuperLearner)
library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "MASS")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

data(Boston, package = "MASS")

set.seed(1)
sl = SuperLearner(Boston$medv, subset(Boston, select = -medv),
                  family = gaussian(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.glm"))

sl

sl_stderr(sl, y = Boston$medv)
