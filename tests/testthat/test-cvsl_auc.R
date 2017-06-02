library(ck37r)
library(SuperLearner)
library(testthat)

context("CVSL auc")

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Subset rows to speed up example computation.
row_subset = sample(nrow(Boston), 400)

Boston = Boston[row_subset, ]

# Outcome needs to be a binary variable.
X = subset(Boston, select = -chas)

cvsl = CV.SuperLearner(Boston$chas, X[, 1:3], family = binomial(),
                       cvControl = list(V = 2, stratifyCV = T),
                       SL.library = c("SL.mean", "SL.glm"))
cvsl_auc(cvsl)

# TODO: cause an error in cvAUC::ci.cvAUC for testing purposes.
