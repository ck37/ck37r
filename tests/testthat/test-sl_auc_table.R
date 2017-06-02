library(ck37r)
library(SuperLearner)
library(testthat)

context("SL auc table")

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Subset rows to speed up example computation.
row_subset = sample(nrow(Boston), 200)

Boston = Boston[row_subset, ]

# Outcome needs to be a binary variable.
X = subset(Boston, select = -chas)

sl = SuperLearner(Boston$chas, X[, 1:3], family = binomial(),
                  cvControl = list(V = 2, stratifyCV = T),
                  SL.library = c("SL.mean", "SL.glm"))

sl_auc_table(sl, Y = Boston$chas)
