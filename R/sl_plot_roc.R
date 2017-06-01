#' @title Plot a ROC curve from cross-validated AUC from SuperLearner
#' @description
#' Plots the ROC curve for a single learner from a SuperLearner object,
#' defaulting to the minimum estimated risk learner. Based on code by Alan Hubbard.
#'
#' @param sl SuperLearner object
#' @param Y Outcome vector if not already included in the SL object.
#' @param learner Which learner to plot - defaults to minimum risk learner.
#' @param title Title to use in the plot.
#' @param digits Digits to use when rounding AUC and CI for plot.
#'
#' @return List with plotted AUC & CI, table of AUC results for all learners, and
#'    the name of the best learner.
#'
#' @examples
#'
#' library(SuperLearner)
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' set.seed(1)
#' sl = SuperLearner(Boston$chas, subset(Boston, select = -chas),
#'                   family = binomial(), SL.library = c("SL.mean", "SL.glm"),
#'                   cvControl = list(V = 2))
#'
#' sl
#'
#' sl_plot_roc(sl, Y = Boston$chas)
#'
#' @references
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
#' @importFrom methods slot
#' @importFrom ROCR prediction performance
#' @importFrom ggplot2 qplot theme_bw annotate
#'
#' @export
sl_plot_roc = function(sl,
           Y = sl$Y,
           learner = which.min(sl$cvRisk),
           title = paste0("SuperLearner cross-validated ROC: ",
                          names(sl$cvRisk)[learner]),
           digits = 4) {

  preds = sl$Z[, learner]
  pred = ROCR::prediction(preds, Y)
  perf1 = ROCR::performance(pred, "sens", "spec")

  auc_table = ck37r::sl_auc_table(sl, Y)

  # We need to index using learner name because auc_table() has been sorted.
  learner_name = names(sl$cvRisk)[learner]

  auc = auc_table[learner_name, "auc"]
  ci_upper = auc_table[learner_name, "ci_upper"]
  ci_lower = auc_table[learner_name, "ci_lower"]

  txt = paste0("AUC = ",
               sprintf(paste0("%0.", digits, "f"), round(auc, digits)),
               "\n95% CI = ", sprintf("%0.3f", round(ci_lower, digits)),
               " - ", sprintf("%0.3f", round(ci_upper, digits)))

  # ggplot version.
  print(ggplot2::qplot(1 - methods::slot(perf1,"x.values")[[1]],
                       methods::slot(perf1,"y.values")[[1]],
                       xlab = "1 - Specificity (false positives)",
                       ylab = "Sensitivity (true positives)",
                       geom = "line",
                       main = title) +
          ggplot2::theme_bw() +
          ggplot2::annotate("text", x = 0.63, y = 0.15, label = txt, size = 6) +
          ggplot2::annotate("segment", x = 0, xend = 1, y = 0, yend = 1))

  # Return AUC, AUC CI, and full table of AUC results.
  results = list(auc = auc,
                 auc_ci = c(ci_lower, ci_upper),
                 auc_table = auc_table,
                 learner_best = learner_name)

  # Return results invisibly.
  invisible(results)
}
