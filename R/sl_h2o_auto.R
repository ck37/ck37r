# PR to be added for SuperLearner package sometime soon.
# By Chris Kennedy based on wrapper code by Erin LeDell.
# TODO: add roxygen and test.
#' Automatic machine learning using h2o
#'
#' Requires a recent version of h2o that has h2o.automl()
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Optional dataframe to predict the outcome
#' @param obsWeights Optional observation-level weights (supported but not tested)
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification, "multinomial" for multiple classification (not yet supported).
#' @param nthreads Number of threads to use, if h2o cluster not alreay started.
#' @param max_runtime_secs Maximum runtime in seconds, does not yield reproducible results.
#' @param max_models Maximum number of models to fit, key parameter to improve performance.
#' @param stopping_metric Metric to optimize towards.
#' @param stopping_rounds Stop if metric does not improve for this many consecutive rounds.
#' @param nfolds # of CV folds for internal cross-validation.
#' @param verbose If TRUE display extra output.
#' @param ... Any remaining arguments, not used.
#' @examples
#
#' # Enable data.table h2o import, which should be faster.
#' # Make sure data.table and slam R packages are installed too.
#' options("h2o.use.data.table" = TRUE)
#
#' \dontrun{
#' library(h2o)
#' # Start an h2o server with all (physical) cores usable.
#' local_h2o = h2o.init(nthreads = RhpcBLASctl::get_num_cores(),
#'                      # May need to specify extra memory.
#'                       max_mem_size = "8g")
#'
#' library(SuperLearner)
#' h2o_auto = create.Learner("SL.h2o_auto",
#'         # Increase max models and stopping rounds for better models.
#'         # Decrease nfolds for faster training but less certainty.
#'                           params = list(max_models = 30,
#'                                         stopping_rounds = 5,
#'                                         nfolds = 10))
#' sl =
#'   SuperLearner(Y = Y, X = X,
#'                family = binomial(),
#'                SL.library = c("SL.mean", h2o_auto$names),
#'                verbose = T,
#'                # Stratify during CV in case of rare outcome.
#'                cvControl = SuperLearner.CV.control(V = 10L, stratifyCV = T))
#' print(sl)
#'
#' h2o.shutdown()
#' }
#' @importFrom h2o h2o.init h2o.cbind as.h2o h2o.predict h2o.rm
#' @importFrom SuperLearner CVFolds
#' @export
SL.h2o_auto <- function(Y, X, newX, family, obsWeights, id,
                        nthreads = 1,
                        #max_runtime_secs = 600,
                        max_runtime_secs = NULL,
                        max_models = 20,
                        stopping_metric = NULL,
                        stopping_rounds = 7,
                        nfolds = 10,
                        verbose = T,
                        ...) {
  #.SL.require("h2o")
  requireNamespace("h2o")

  # Start h2o server is one hasn't already been started, otherwise
  # connect to an existing server. Ideally the server would have
  # been started manually prior to running SL though, to allow
  # greater customization.
  h2o.init(nthreads = nthreads)

  if (verbose) {
    cat("Creating internal folds.\n")
  }

  # Via http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/fold_column.html
  # Create a fold column with K folds.
  # Randomly assign fold numbers 0 through 9 for each row in the column.
  # This doesn't support stratified CV, which we need for rare outcomes.
  #fold_numbers = h2o.kfold_column(train, nfolds = nfolds, seed = -1)
  cvControl = list(V = nfolds, stratifyCV = F, shuffle = T)

  # Check if id is distinct for each observation, meaning we don't need to use it.
  if (length(unique(id)) == nrow(X) && family$family == "binomial") {
    # We don't actually have a real grouping id, set to id NULL so that
    # we can stratify internal CV folds.
    id = NULL
    # If we're doing classification we can turn on stratification.
    cvControl$stratifyCV = T
  }
  # This returns a list with nfolds elements.
  raw_folds = CVFolds(N = nrow(X), id = id, Y = Y, cvControl = cvControl)

  # Convert raw_folds into a vector of fold numbers.
  fold_nums = rep(NA, nrow(X))
  for (fold_i in seq_along(raw_folds)) {
    fold_nums[raw_folds[[fold_i]]] = fold_i
  }

  # Upload into h2o.
  cv_folds = as.h2o(fold_nums)

  # Rename CV folds.
  cv_fold_name = "_cv_fold_numbers"
  names(cv_folds) = cv_fold_name

  # Print the fold_assignment column.
  if (F && verbose) {
    cat("CV Fold distribution:\n")
    print(table(as.vector(cv_folds), useNA = "ifany"))
  }

  # Upload data to H2O
  if (verbose) {
    cat("Converting training dataframe to h2o object.\n")
  }

  # TODO: check if this fails, e.g. due to lack of memory.
  # TODO: support obsWeights.
  y_name = "_y"
  train <- as.h2o(cbind(Y, X))#, `_obsWeights` = obsWeights))

  x <- names(X)
  names(train)[1] = y_name

  # Append the cv_folds column to the training dataset.
  train <- h2o.cbind(train, cv_folds)

  if (family$family == "binomial") {
    train[, y_name] <- h2o::as.factor(train[, y_name])
    if (is.null(stopping_metric)) {
      stopping_metric = "AUC"
    }
  }

  if (verbose) {
    cat("Running h2o.automl().\n")
  }

  fit <- h2o::h2o.automl(y = y_name, x = x, training_frame = train,
                         fold_column = cv_fold_name,
                         #weights_column = "_obsWeights",
                         max_runtime_secs = max_runtime_secs,
                         max_models = max_models,
                         stopping_rounds = stopping_rounds,
                         stopping_metric = stopping_metric)

  lb = fit@leaderboard
  if (verbose) {
    print(lb)
  }

  # Upload test data to H2O.
  if (verbose) {
    cat("Converting test dataframe to h2o object.\n")
  }
  test <- as.h2o(newX)

  if (verbose) {
    cat("Predicting on test data.\n")
  }

  # NOTE: this will yield a warning about missing column "_cv_fold_numbers".
  suppressWarnings({
    h2o_pred <- h2o.predict(object = fit, newdata = test)
  })

  # Clean up h2o objects to try to conserve memory.
  h2o.rm(train)
  h2o.rm(test)
  h2o.rm(cv_folds)

  #fit <- list(object = fit)
  class(fit) <- "SL.h2o"

  if (family$family == "binomial") {
    pred = as.vector(as.data.frame(h2o_pred)[, 3])
  } else {
    pred = as.vector(as.data.frame(h2o_pred)[, 1])
  }

  # Clean up h2o objects.
  h2o.rm(h2o_pred)

  out <- list(pred = pred, fit = fit)
  return(out)
}
