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
summary(X)

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

# Check prediction.
preds = predict(sl, X)
summary(preds$pred)

######
# Dynamic stratification example.

# First see what a decision tree gives us.
rpart = SL.rpart(Y = Y_gaus, X = X, newX = X, family = gaussian(), obsWeights = rep(1, length(Y_gaus)))
(rpart_pruned = rpart::prune.rpart(rpart$fit$object,
                                   cp = rpart$fit$object$cptable[4, "CP"]))

# Function to make 4 custom strata given a dataframe with the appropriate variables
make_strata = function(df, verbose = FALSE) {
  df$custom_strata =
    with(df,
       ifelse(rm < 6.941,
         ifelse(lstat >= 14.4, 0, 1),
         ifelse(rm < 7.437, 2, 3)))
  if (verbose) {
    cat("Strata frequencies:\n")
    print(table(df$custom_strata))

  }
  df
}

x2 = make_strata(X, verbose = TRUE)

SL.stratified.custom =
  function(Y, X, newX, ...) {
  # Create stratification variable on X (training data)
  X = make_strata(X)

  # Create stratification variable on newX (test data)
  newX = make_strata(newX)

  # Pass-through the function arguments to use the existing wrapper to do the rest.
  SL.stratified(Y = Y, X = X, newX = newX, stratify_on = "custom_strata", ...)
  }

# Create an example training and test set.
set.seed(1)
train_rows = sample(nrow(X), ceiling(nrow(X) * 0.8))
x_train = X[train_rows, ]
y_train = Y_bin[train_rows]
x_test = X[-train_rows, ]
y_test = Y_bin[-train_rows]

# Run a single training/test split using our wrapper and uniform weighting.
strat_obj = SL.stratified.custom(y_train, x_train, x_test,
                               obsWeights = rep(1, length(y_train)))
strat_obj$fit$object

train_strata = cbind(x_train, y_train) %>%
  dplyr::mutate(stratum0 = as.integer(rm < 6.941 & lstat >= 14.4),
         stratum1 = as.integer(rm < 6.941 & lstat < 14.4),
         stratum2 = as.integer(rm >= 6.941 & rm < 7.437),
         stratum3 = as.integer(rm >= 6.941 & rm >= 7.437))

# Check stratification creation on the training set.
table(make_strata(x_train)$custom_strata, useNA = "ifany")

# Should be 140 1s - correct.
table(train_strata$stratum0)
# Should be 202 1s - correct.
table(train_strata$stratum1)
# Should be 38 1s - correct.
table(train_strata$stratum2)
# Should be 25 1s - correct.
table(train_strata$stratum3)

# Now examine test set stratification.
# Compare to manual calculations:
test_strata = cbind(x_test, y_test) %>%
  dplyr::mutate(stratum0 = as.integer(rm < 6.941 & lstat >= 14.4),
         stratum1 = as.integer(rm < 6.941 & lstat < 14.4),
         stratum2 = as.integer(rm >= 6.941 & rm < 7.437),
         stratum3 = as.integer(rm >= 6.941 & rm >= 7.437))

# Check stratification creation on the test set.
table(make_strata(x_test)$custom_strata, useNA = "ifany")

# Should be 35 1s - correct.
table(test_strata$stratum0)
# Should be 53 1s - correct.
table(test_strata$stratum1)
# Should be 8 1s - correct.
table(test_strata$stratum2)
# Should be 5 1s - correct.
table(test_strata$stratum3)


##################
# Calculate the stratified outcome means for comparison to the wrapper.
# Training set first.
(train_means =
    train_strata %>%
  # Here is one way to get stratum-specific means. Otherwise should reshape
  # from wide to long, etc.
  dplyr::summarize(pred0 = mean(ifelse(stratum0 == 1, y_train, NA), na.rm = TRUE),
            pred1 = mean(ifelse(stratum1 == 1, y_train, NA), na.rm = TRUE),
            pred2 = mean(ifelse(stratum2 == 1, y_train, NA), na.rm = TRUE),
            pred3 = mean(ifelse(stratum3 == 1, y_train, NA), na.rm = TRUE)))

# Compare to our wrapper:
strat_obj$fit$object

# Or display side by side:
data.frame(manual = as.vector(t(train_means[1, ])),
           wrapper = strat_obj$fit$object$`_pred`,
           diff = as.vector(t(train_means[1, ])) - strat_obj$fit$object$`_pred`)

# What is the AUC on the training set? 0.8406
# Run the wrapper again but pass x_train as the test set.
pROC::auc(y_train, SL.stratified.custom(y_train, x_train, x_train,
                                       obsWeights = rep(1, length(y_train)))$pred)

# Calculate the stratified outcome test means for comparison to the wrapper.
(test_means =
    test_strata %>%
  # Here is one way to get stratum-specific means. Otherwise should reshape
  # from wide to long, etc.
  dplyr::summarize(pred0 = mean(ifelse(stratum0 == 1, y_test, NA), na.rm = TRUE),
            pred1 = mean(ifelse(stratum1 == 1, y_test, NA), na.rm = TRUE),
            pred2 = mean(ifelse(stratum2 == 1, y_test, NA), na.rm = TRUE),
            pred3 = mean(ifelse(stratum3 == 1, y_test, NA), na.rm = TRUE)))

# Compare to our wrapper's predictions:
#table(sl_test$pred)


# What is the AUC on the test set? 0.7894
#pROC::auc(y_test, sl_test$pred)

##
# Test a full SuperLearner run.
sl = SuperLearner(Y = Y_bin, X = X,
                  family = "binomial",
                  SL.library = c("SL.mean", "SL.stratified.custom"))

# 99% weight on the custom stratified estimator, far lower risk than the mean.
sl

