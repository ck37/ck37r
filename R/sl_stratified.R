# TODO: PR for SuperLearner

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
SL.stratified = function(Y, X, newX, family, obsWeights, id, stratify_on, ...) {

  # Take the mean of Y over specific strata of X.
  stratified_pred =
    cbind(Y, X) %>% dplyr::group_by_at(stratify_on) %>%
    # We prefix with an underscore to minimize any conflict with column names in X.
    dplyr::mutate(`_pred` = mean(Y, na.rm = TRUE),
                  `_size` = dplyr::n()) %>%
    # Restrict to one row per stratum
    dplyr::filter(dplyr::row_number() == 1) %>%
    # We only need the stratum and the prediction.
    # Make sure to include the grouping cols to avoid a warning message.
    dplyr::select(dplyr::group_cols(), `_pred`, `_size`) %>% as.data.frame()

  # Now left_join with newX to generate prediction.
  preds = dplyr::left_join(newX, stratified_pred, by = stratify_on) %>% as.data.frame()

  # Replace any empty cells with the sample mean.
  # TODO: remove the highest cardinality grouping variable and see if that stratification solves it.
  missing_pred = is.na(preds$`_pred`)
  if (any(missing_pred)) {
    preds[missing_pred, "_pred"] = mean(Y, na.rm = TRUE)
  }

  # fit returns all objects needed for predict()
  fit = list(object = stratified_pred)

  # Declare class of fit for predict()
  class(fit) = 'SL.stratified'

  # Return the result.
  out = list(pred = preds$`_pred`, fit = fit)
  return(out)
}

# TODO: make a predict() method for SL.stratified.
