#' @title Plot a ROC curve from cross-validated AUC from SuperLearner
#' @description
#' Thank you to Alan Hubbard for the initial code.
#' @param sl SuperLearner object
#' @param learner Which learner to plot - defaults to minimum risk learner.
#' @param Y tbd
#' @param title tbd
#' @param digits tbd
#' @references
#' Add Erin LeDell paper/chapter.
#'
#' @importFrom methods slot
#' @importFrom ROCR prediction performance
#' @importFrom ggplot2 qplot theme_bw annotate
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

  auc_table = ck37r::sl_auc(sl, Y)

  auc = auc_table[learner, "auc"]
  ci_upper = auc_table[learner, "ci_upper"]
  ci_lower = auc_table[learner, "ci_lower"]

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
                 auc_table = auc_table)

  # Return results invisibly.
  invisible(results)
}
