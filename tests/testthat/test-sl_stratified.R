library(ck37r)
library(SuperLearner)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("Learner: SL.stratified")

############################
# Setup test dataset.

data(Boston, package = "MASS")

set.seed(1)

# Sample 100 random observations to speed up testing.
#Boston = Boston[sample(nrow(Boston), 300), ]

Y_gaus = Boston$medv
Y_bin = as.numeric(Boston$medv > 23)

# Remove outcome from covariate dataframe.
X = Boston[, -14]

# Convert to a matrix and remove intercept.
X_mat = model.matrix(~ ., data = X)[, -1]

#####################
# Check wrapper fit, prediction, and basic SuperLearner.
####

colnames(X_mat)
summary(lm(Y_gaus ~ ., data = X))

str(Boston)

##########
# Try just the wrapper itself, not via SuperLearner
model = SL.stratified(Y_gaus, X, X, family = gaussian(),
                obsWeights = rep(1, nrow(X)), stratify_on = "chas")
model$fit$object

model = SL.stratified(Y_bin, X, X, family = binomial(), obsWeights = rep(1, nrow(X)),
                stratify_on = "chas")

test_fn = function(...) SL.stratified(stratify_on = "chas", ...)

sl_lib = c("test_fn", "SL.mean")

sl = SuperLearner(Y = Y_bin, X = X, SL.library = sl_lib,
                  cvControl = list(V = 2L),
                  family = binomial())
print(sl)
rm(sl_lib)
