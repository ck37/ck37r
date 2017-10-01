#' @title Plot a ROC curve from cross-validated AUC from CV.SuperLearner
#' @description
#' Based on initial code by Alan Hubbard.
#'
#' @param x CV.SuperLearner object
#' @param y Outcome vector if not already included in the SL object.
#' @param title Title to use in the plot.
#' @param subtitle Optional plot subtitle.
#' @param digits Digits to use when rounding AUC and CI for plot.
#' @param show_plot If TRUE print the ggplot, otherwise just return in list.
#'
#' @return List with the AUC plus standard error and confidence interval.
#'
#' @examples
#'
#' library(SuperLearner)
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' set.seed(1, "L'Ecuyer-CMRG")
#'
#' # Subset rows to speed up example computation.
#' row_subset = sample(nrow(Boston), 100)
#'
#' Boston = Boston[row_subset, ]
#' X = subset(Boston, select = -chas)
#'
#' cvsl = CV.SuperLearner(Boston$chas, X[, 1:2], family = binomial(),
#'                       cvControl = list(V = 2, stratifyCV = TRUE),
#'                       SL.library = c("SL.mean", "SL.glm"))
#' plot_roc(cvsl)
#'
#' @references
#'
#' LeDell, E., Petersen, M., & van der Laan, M. (2015). Computationally
#' efficient confidence intervals for cross-validated area under the ROC curve
#' estimates. Electronic journal of statistics, 9(1), 1583.
#'
#' Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
#' Division of Biostatistics Working Paper Series. Paper 226.
#' http://biostats.bepress.com/ucbbiostat/paper266/
#'
#' Sing, T., Sander, O., Beerenwinkel, N., & Lengauer, T. (2005). ROCR:
#' visualizing classifier performance in R. Bioinformatics, 21(20), 3940-3941.
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#' http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
#'
#' @seealso \code{\link{cvsl_auc}} \code{\link{sl_plot_roc}}
#'   \code{\link[cvAUC]{ci.cvAUC}}
#'
#' @method plot_roc CV.SuperLearner
#'
#' @export
plot_roc.CV.SuperLearner = function(x, y = cvsl$Y,
                         title = "CV-SuperLearner cross-validated ROC",
                         subtitle = "",
                         digits = 4,
                         show_plot = TRUE) {
  # Change to a better object name.
  cvsl = x

  preds = cvsl$SL.predict
  pred = ROCR::prediction(preds, y)
  perf1 = ROCR::performance(pred, "sens", "spec")

  ciout = ck37r::cvsl_auc(cvsl)

  txt = paste0("AUC = ",
               sprintf(paste0("%0.", digits, "f"), round(ciout$cvAUC, digits)),
               "\n95% CI = ", sprintf("%0.3f", round(ciout$ci[1], digits)),
               " - ", sprintf("%0.3f", round(ciout$ci[2], digits)))

  # ggplot version.
  p = ggplot2::qplot(1 - methods::slot(perf1, "x.values")[[1]],
                       methods::slot(perf1, "y.values")[[1]],
                       xlab = "False positive % (1 - specificity)",
                       ylab = "True positive % (sensitivity)",
                       geom = "line",
                       main = title) +
          ggplot2::labs(subtitle = subtitle) +
          ggplot2::theme_bw() +
          ggplot2::annotate("text", x = 0.63, y = 0.15, label = txt, size = 6) +
          ggplot2::annotate("segment", x = 0, xend = 1, y = 0, yend = 1)

  if (show_plot) {
    print(p)
  }

  # Return AUC, AUC CI, and ggplot object.
  results = list(auc = ciout$cvAUC,
                 auc_se = ciout$se,
                 auc_ci = ciout$ci,
                 plot = p)

  # Return results invisibly.
  invisible(results)
}