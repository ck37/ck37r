# TODO: PR for SuperLearner
# Avoid a note about "no visible binding for global variable".
utils::globalVariables(c("_pred", "_size"))

#' @title Stratified estimator
#'
#' @description This estimator stratifies the dataset on the values of the
#' specified variables and predicts the outcome mean within each cell. It is
#' intended as a low-variance, high bias estimator that can often provide
#' better predictions than the overall outcome mean.
#'
#' @param Y Outcome variable
#' @param X Covariate dataframe
#' @param newX Dataframe to predict the outcome
#' @param obsWeights Optional observation-level weights
#' @param id Optional id to group observations from the same unit (not used
#'   currently).
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification. Untested options: "multinomial" for multiple classification
#'   or "mgaussian" for multiple response, "poisson" for non-negative outcome
#'   with proportional mean and variance, "cox".
#' @param stratify_on Vector of variables used to create stratification cells,
#'   e.g. c('age', 'gender').
#' @param ... Any other arguments, not used.
#' @importFrom dplyr group_by_at summarize
#' @importFrom stats weighted.mean
#' @export
SL.stratified = function(Y, X, newX, family, obsWeights, id, stratify_on, ...) {

  # Take the (possibly weighted) mean of Y over specific strata of X.
  stratified_pred =
    cbind(Y, X, obsWeights) %>% dplyr::group_by_at(stratify_on) %>%
    # We prefix with an underscore to minimize any conflict with column names in X.
    dplyr::summarize(`_pred` = stats::weighted.mean(Y, w = obsWeights, na.rm = TRUE),
                     `_size` = sum(!is.na(Y) * obsWeights, na.rm = TRUE)) %>%
      as.data.frame()

  # Now left_join with newX to generate prediction.
  preds = dplyr::left_join(newX, stratified_pred, by = stratify_on) %>% as.data.frame()

  # Replace any empty cells with the sample mean.
  # TODO: remove the highest cardinality grouping variable and see if that stratification solves it.
  sample_mean = stats::weighted.mean(Y, w = obsWeights, na.rm = TRUE)
  missing_pred = is.na(preds$`_pred`)
  if (any(missing_pred)) {
    preds[missing_pred, "_pred"] = sample_mean
  }

  # fit returns all objects needed for predict()
  fit = list(object = stratified_pred,
             stratify_on = stratify_on,
             sample_mean = sample_mean)

  # Declare class of fit for predict()
  class(fit) = 'SL.stratified'

  # Return the result.
  out = list(pred = preds$`_pred`, fit = fit)
  return(out)
}

#' @title predict() for SL.stratified
#'
#' @param object SuperLearner object
#' @param newdata Dataframe to predict the outcome
#' @param family "gaussian" for regression, "binomial" for binary
#'   classification. (not useD)
#' @param ... Additional arguments (not used)
#'
#' @export
predict.SL.stratified =
  function(object, newdata, family, ...) {

    #stratify_on = setdiff(names(object$object), c("_pred", "_size"))

    # Now left_join with newX to generate prediction.
    preds = dplyr::left_join(newdata, object$object,
                             by = object$stratify_on) %>% as.data.frame()

    # Replace any empty cells with the sample mean.
    # TODO: remove the highest cardinality grouping variable and see if that stratification solves it.
    missing_pred = is.na(preds$`_pred`)
    if (any(missing_pred)) {
      preds[missing_pred, "_pred"] = object$sample_mean
    }

    return(preds$`_pred`)
  }

