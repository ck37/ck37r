library(ck37r)
library(SuperLearner)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("SL RMSE table")

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Subset rows to speed up example computation.
row_subset = sample(nrow(Boston), 200)

Boston = Boston[row_subset, ]

# Outcome should be continuous.
X = subset(Boston, select = -age)

sl = SuperLearner(Boston$age, X[, 1:3], family = gaussian(),
                  cvControl = list(V = 2L),
                  SL.library = c("SL.mean", "SL.glm"))

debugonce(rmse)
rmse_table(sl, y = Boston$age)
