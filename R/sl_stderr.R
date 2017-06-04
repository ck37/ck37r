#' @title Calculate the SE of individual SL learners
#'
#' @description
#' This will help understand risk estimates of learners in SL, similar to CV.SL.
#'
#' @param sl SuperLearner result object
#' @param y Outcome vector
#' @param obsWeights Observation weights
#'
#' @return Vector of the standard errors of the risk estimate for each learner
#'   in the SL object.
#'
#' @examples
#'
#' library(SuperLearner)
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' set.seed(1)
#' sl = SuperLearner(Boston$medv, subset(Boston, select = -medv),
#'                   family = gaussian(), cvControl = list(V = 2),
#'                   SL.library = c("SL.mean", "SL.glm"))
#'
#' sl
#'
#' sl_stderr(sl, y = Boston$medv)
#'
#' @seealso \code{\link{plot.SuperLearner}}
#'    \code{\link[SuperLearner]{summary.CV.SuperLearner}}
#'
#'
#' @references
#'
#' Dudoit, S., & van der Laan, M. J. (2005). Asymptotics of cross-validated risk
#' estimation in estimator selection and performance assessment. Statistical
#' Methodology, 2(2), 131-154.
#'
#' Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
#' Division of Biostatistics Working Paper Series. Paper 226.
#' http://biostats.bepress.com/ucbbiostat/paper266/
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#' http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
#'
#' @export
sl_stderr = function(sl, y, obsWeights = rep(1, length(y))) {
  # Each column is sl$Z is a learner's prediction.
  # We can use that to calculate the loss on each observation.

  # See Dudoit and vDL 2005 section 4.3 and Polley & vdL 2010 equation 5.
  # And https://github.com/ecpolley/SuperLearner/blob/master/R/summary.CV.SuperLearner.R
  SEs = apply(sl$Z, MARGIN = 2, FUN = function(predicted) {
    sd(obsWeights * (y - predicted)^2) / sqrt(length(y))
  })

  names(SEs) = names(sl$cvRisk)

  # Return the standard errors of the learner predictions.
  SEs
}
