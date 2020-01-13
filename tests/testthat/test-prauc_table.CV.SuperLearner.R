library(ck37r)
library(SuperLearner)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                character.only = TRUE))))
  return()

context("CV.SL prauc table")

data(Boston, package = "MASS")

set.seed(1)

# This will generate some warnings about glm fitted probabilities.
suppressWarnings({
  cvsl = CV.SuperLearner(Boston$chas, subset(Boston, select = -chas),
                         family = binomial(),
                         cvControl = list(V = 2, stratifyCV = T),
                         SL.library = c("SL.mean", "SL.glm"))
})
summary(cvsl)

prauc_table(cvsl, y = Boston$chas)
