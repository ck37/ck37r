library(ck37r)
library(SuperLearner)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "MASS")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("vim_corr")

data(Boston, package = "MASS")

set.seed(1, "L'Ecuyer-CMRG")

# Subset rows to speed up example computation.
row_subset = sample(nrow(Boston), 400)

Boston = Boston[row_subset, ]

# Test binary outcome
#df = subset(Boston, select = -chas)
df = Boston

names(df)

result = vim_corr(setdiff(names(df), "chas"), df, "chas",
                  bootse = FALSE,
                  verbose = TRUE)
result
