#' Calculate the SE of individual SL learners
#'
#' This will help understand risk estimates of learners in SL, similar to CV.SL.
#'
#' @param sl SuperLearner result object
#' @param Y Outcome vector
#' @param obsWeights Observation weights
sl_stderr = function(sl, Y, obsWeights = rep(1, length(Y))) {
  # Each column is sl$Z is a learner's prediction.
  # We can use that to calculate the loss on each observation.

  # See equation 5 in Polley & vdL, SuperLearner in Prediction.
  # And https://github.com/ecpolley/SuperLearner/blob/master/R/summary.CV.SuperLearner.R
  SEs = apply(sl$Z, MARGIN = 2, FUN = function(predicted) {
    sd(obsWeights * (Y - predicted)^2) / sqrt(length(Y))
  })

  # Return the standard errors of the learner predictions.
  SEs
}
