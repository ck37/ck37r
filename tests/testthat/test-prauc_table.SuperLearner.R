library(ck37r)
library(SuperLearner)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("SL prauc table")

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Subset rows to speed up example computation.
row_subset = sample(nrow(Boston), 200)

Boston = Boston[row_subset, ]

# Outcome needs to be a binary variable.
X = subset(Boston, select = -chas)

sl = SuperLearner(Boston$chas, X[, 1:3], family = binomial(),
                  cvControl = list(V = 2L, stratifyCV = TRUE),
                  SL.library = c("SL.mean", "SL.glm"))

prauc_table(sl, y = Boston$chas)
