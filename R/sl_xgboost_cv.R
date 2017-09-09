# TODO: add roxygen
# TODO: PR for SuperLearner
# TODO: soup up family = gaussian() / integrate with family = binomial()
# TOOD: add example
#' @noRd
# @importFrom xgboost xgboost xgb.cv
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
           stratified = T,
           # Focus on AUC and see if we do better.
           eval_metric = ifelse(family$family == "binomial", "auc", "rmse"),
           print_every_n = 400L,
           # Also set 4 threads while we're at it.
           nthread = 4,
           verbose = 0, save_period = NULL, ...) {
    #.SL.require("xgboost")
    if (utils::packageVersion("xgboost") < 0.6)
      stop("SL.xgboost requires xgboost version >= 0.6, try help('SL.xgboost') for details")

    if (!is.matrix(X)) {
      # Convert to a matrix, then remove the added intercept column.
      X = stats::model.matrix(~ ., X)[, -1]
    }

    xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)

    if (family$family == "gaussian") {
      model = xgboost::xgboost(data = xgmat, objective = "reg:linear",
                               nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode,
                               eta = shrinkage, verbose = verbose, nthread = nthread,
                               save_period = save_period)
    }
    if (family$family == "binomial") {
      model_cv =
        xgboost::xgb.cv(data = xgmat, objective = "binary:logistic",
                        nrounds = ntrees,
                        max_depth = max_depth,
                        min_child_weight = minobspernode,
                        eta = shrinkage,
                        verbose = 1L,
                        nthread = nthread,
                        subsample = subsample,
                        colsample_bytree = colsample_bytree,
                        gamma = gamma,
                        early_stopping_rounds = early_stopping_rounds,
                        stratified = stratified,
                        print_every_n = print_every_n,
                        metrics = list(eval_metric),
                        nfold = nfold,
                        save_period = save_period)

      print(model_cv)

      # Refit that model using the best params.
      model =
        xgboost::xgboost(data = xgmat, #objective = "binary:logistic",
                         nrounds = model_cv$best_ntreelimit,
                         params = model_cv$params,
                         verbose = 0L,
                         print_every_n = 500L,
                         save_period = save_period)
    }
    if (family$family == "multinomial") {
      model = xgboost::xgboost(data = xgmat, objective = "multi:softmax",
                               nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode,
                               eta = shrinkage, verbose = verbose, num_class = length(unique(Y)),
                               nthread = nthread, save_period = save_period)
    }
    if (!is.matrix(newX)) {
      newX = stats::model.matrix(~ ., newX)[, -1]
    }
    #pred = xgboost::predict(model, newdata = newX)
    pred = xgboost:::predict.xgb.Booster(model, newdata = newX)
    fit = list(object = model)
    class(fit) = c("SL.xgboost")
    out = list(pred = pred, fit = fit)
    return(out)
}
