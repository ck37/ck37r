# TODO: PR for SuperLearner
# TODO: test gaussian family
# TOOD: add example
#' XGBoost SuperLearner wrapper with internal cross-validation for early-stopping
#'
#' Supports the Extreme Gradient Boosting package for SuperLearnering, which is
#' a variant of gradient boosted machines (GBM). Conducts internal cross-validation
#' and stops when performance plateaus.
#'
#' The performance of XGBoost, like GBM, is sensitive to the configuration
#' settings. Therefore it is best to create multiple configurations using
#' create.SL.xgboost and allow the SuperLearner to choose the best weights based
#' on cross-validated performance.
#'
#' If you run into errors please first try installing the latest version of
#' XGBoost from CRAN.
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Optional dataframe to predict the outcome
#' @param obsWeights Optional observation-level weights (supported but not tested)
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification, "multinomial" for multiple classification (not yet supported).
#' @param ntrees How many trees to fit. Low numbers may underfit but high
#'   numbers may overfit, depending also on the shrinkage.
#' @param early_stopping_rounds If performance has not improved in this many
#'   rounds, stop.
#' @param nfold Number of internal cross-validation folds.
#' @param max_depth How deep each tree can be. 1 means no interactions, aka tree
#'   stubs.
#' @param shrinkage How much to shrink the predictions, in order to reduce
#'   overfitting.
#' @param minobspernode Minimum observations allowed per tree node, after which
#'   no more splitting will occur.
#' @param subsample Observation sub-sampling, to reduce tree correlation.
#' @param colsample_bytree Column sub-sampling, to reduce tree correlation.
#' @param gamma Metric for node-splitting, higher values result in less complex trees.
#' @param stratified If stratified sampling should be used for binary outcomes, defaults
#' to T.
#' @param eval_metric Metric to use for early-stopping, defaults to AUC for
#' classification and RMSE for regression.
#' @param print_every_n Print estimation status every n rounds.
#' @param nthread How many threads (cores) should xgboost use. Generally we want
#'   to keep this to 1 so that XGBoost does not compete with SuperLearner
#'   parallelization.
#' @param verbose Verbosity of XGB fitting.
#' @param save_period How often (in tree iterations) to save current model to
#'   disk during processing. If NULL does not save model, and if 0 saves model
#'   at the end.
#' @param ... Any remaining arguments (not used).
# @importFrom xgboost xgboost xgb.cv
# @examples
#' @export
SL.xgboost_cv =
  function(Y, X, newX, family, obsWeights, id,
           ntrees = 5000L,
           early_stopping_rounds = 200L,
           nfold = 5L,
           max_depth = 4L,
           shrinkage = 0.1,
           minobspernode = 10L,
           subsample = 0.7,
           colsample_bytree = 0.8,
           gamma = 5,
           # Due to class imbalance,
           stratified = family$family == "binomial",
           # Focus on AUC and see if we do better.
           eval_metric = ifelse(family$family == "binomial", "auc", "rmse"),
           print_every_n = 400L,
           nthread = getOption("sl.cores", 1L),
           verbose = 0,
           save_period = NULL, ...) {

    #.SL.require("xgboost")
    if (utils::packageVersion("xgboost") < 0.6)
      stop("SL.xgboost requires xgboost version >= 0.6, try help('SL.xgboost') for details")

    if (!is.matrix(X)) {
      # Convert to a matrix, then remove the added intercept column.
      X = stats::model.matrix(~ ., X)[, -1]
    }

    xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)

    if (family$family == "gaussian") {
      objective = "reg:linear"
    } else if (family$family == "binomial") {
      objective = "binary:logistic"
    }

    model_cv =
      xgboost::xgb.cv(data = xgmat,#
                      objective = objective, #
                      nrounds = ntrees,#
                      nfold = nfold,#
                      metrics = list(eval_metric), #
                      #stratified = stratified,#
                      verbose = verbose,#
                      #print_every_n = print_every_n,#
                      #early_stopping_rounds = early_stopping_rounds, #
                      max_depth = max_depth,
                      min_child_weight = minobspernode,
                      eta = shrinkage,
                      nthread = nthread,
                      subsample = subsample,
                      colsample_bytree = colsample_bytree,
                      gamma = gamma)#,
                      #save_period = save_period)

    if (verbose) {
      print(model_cv)
    }

    # Refit that model using the best params.
    model =
      xgboost::xgboost(data = xgmat,
                       objective = objective,
                       nrounds = model_cv$best_ntreelimit,
                       params = model_cv$params,
                       verbose = 0L,
                       print_every_n = 500L,
                       save_period = save_period)

    if (family$family == "multinomial") {
      # Not used yet.
      model = xgboost::xgboost(data = xgmat, objective = "multi:softmax",
                               nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode,
                               eta = shrinkage, verbose = verbose, num_class = length(unique(Y)),
                               nthread = nthread, save_period = save_period)
    }

    if (!is.matrix(newX)) {
      newX = stats::model.matrix(~ ., newX)[, -1]
    }
    #pred = xgboost::predict(model, newdata = newX)
    pred = stats::predict(model, newdata = newX)
    #pred = xgboost:::predict.xgb.Booster(model, newdata = newX)
    fit = list(object = model)
    class(fit) = c("SL.xgboost")
    out = list(pred = pred, fit = fit)
    return(out)
}
