library(ck37r)
library(SuperLearner)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "mgcv", "MASS")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("Learner: mgcv")

############################
# Setup test dataset.

data(Boston, package = "MASS")

set.seed(1)

# Sample 100 random observations to speed up testing.
Boston = Boston[sample(nrow(Boston), 300), ]

Y_gaus = Boston$medv
Y_bin = as.numeric(Boston$medv > 23)

# Remove outcome from covariate dataframe.
X = Boston[, -14]

# Convert to a matrix and remove intercept.
X_mat = model.matrix(~ ., data = X)[, -1]

#####################
# Check wrapper fit, prediction, and basic SuperLearner.
####

# Unclear if this is still needed.
if (FALSE && isNamespaceLoaded("gam")) {
  # Make a backup
  #old_s = gam:::s
  # Now override gam package's s() during testing.
  #s = function(...) stop("Error: using gam::s() instead of mgcv::s!")

  # Replace gam:::s() with our custom function.
  #assignInNamespace("s", s, "gam")
  #gam:::s
}

##########
# Try just the wrapper itself, not via SuperLearner
model = SL.mgcv(Y_gaus, X, X, family = gaussian(),
                obsWeights = rep(1, nrow(X)),
       # rad is continuous but causes an error (due to collinearity presumably)
                exclude_spline = "rad")
mgcv:::print.gam(model$fit$object)
class(model$fit$object)
print(mgcv::summary.gam(model$fit$object))

model = SL.mgcv(Y_bin, X, X, family = binomial(), obsWeights = rep(1, nrow(X)),
                exclude_spline = "rad")
mgcv::print.gam(model$fit$object)
print(mgcv::summary.gam(model$fit$object))

# Check prediction.
pred = predict(model$fit, X)
summary(pred)

mgcv_learner = create.Learner("SL.mgcv", params = list(exclude_spline = "rad",
                                                       method = "REML"))

#sl_lib = c("SL.mgcv", "SL.ranger", "SL.mean")
sl_lib = c(mgcv_learner$names, "SL.mean")

sl = SuperLearner(Y = Y_bin, X = X, SL.library = sl_lib,
                  cvControl = list(V = 2L),
                  family = binomial())
print(sl)
rm(sl_lib)
