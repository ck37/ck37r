#' @title Plot a ROC curve from cross-validated AUC from CV.SuperLearner
#' @description
#' Thank you to Alan Hubbard for the initial code.
#' @param cvsl tbd
#' @param Y tbd
#' @param title tbd
#' @param digits tbd
#' @references
#' Add Erin LeDell paper/chapter.
#' @importFrom ROCR prediction performance
#' @importFrom methods slot
#' @export
cvsl_plot_roc = function(cvsl, Y = cvsl$Y, title = "CV-SuperLearner Cross-validated ROC",
                         digits = 4) {
  preds = cvsl$SL.predict
  pred = ROCR::prediction(preds, Y)
  perf1 = ROCR::performance(pred, "sens", "spec")

  ciout = ckTools::cvsl_auc(cvsl)

  txt = paste0("AUC = ",
               sprintf(paste0("%0.", digits, "f"), round(ciout$cvAUC, digits)),
               "\n95% CI = ", sprintf("%0.3f", round(ciout$ci[1], digits)),
               " - ", sprintf("%0.3f", round(ciout$ci[2], digits)))

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

  # Return AUC and AUC CI.
  results = list(auc = ciout$cvAUC,
                 auc_ci = ciout$ciout)

  # Return results invisibly.
  invisible(results)
}
