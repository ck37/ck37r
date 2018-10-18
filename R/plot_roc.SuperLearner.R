#' @title Plot a ROC curve from cross-validated AUC from SuperLearner
#' @description
#' Plots the ROC curve for a single learner from a SuperLearner object,
#' defaulting to the minimum estimated risk learner. Based on code by Alan
#' Hubbard.
#'
#' @param x SuperLearner object
#' @param y Outcome vector if not already included in the SL object.
#' @param learner Which learner to plot (numeric index or character string).
#'   Defaults to minimum risk learner.
#' @param title Title to use in the plot.
#' @param subtitle TBD.
#' @param digits Digits to use when rounding AUC and CI for plot.
#' @param ... Any additional unused arguments, due to the auc_table generic.
#'
#' @return List with plotted AUC & CI, table of AUC results for all learners,
#'       and the name of the best learner.
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
#' plot_roc(sl, y = Boston$chas)
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
#' @importFrom ggplot2 qplot theme_bw annotate geom_line
#'
#' @export
plot_roc.SuperLearner =
  function(x,
           y = x$Y,
           learner = NULL,
           title = "SuperLearner cross-validated ROC",
           subtitle = NULL,
           digits = 4,
           ...) {

  # Better object name.
  sl = x

  auc_table = ck37r::auc_table(sl, y)

  # Choose the learner with the highest AUC.
  if (is.null(learner)) {
    # Extract the original learner index based on learner name in the AUC table.
    # Take the first learner if there are ties.
    learner_index = which(names(sl$cvRisk) == auc_table$learner[which.max(auc_table$auc)])[1]
  } else {
    if (class(learner) == "character") {
      # Select the first element in case of multiple learners having the same name.
      learner_index = which(names(sl$cvRisk) == learner)[1]
    } else {
      # Learner argument is already an index.
      learner_index = learner
    }
  }

  preds = sl$Z[, learner_index]
  pred = ROCR::prediction(preds, y)
  perf1 = ROCR::performance(pred, "sens", "spec")

  # We need to index using learner name because auc_table() has been sorted.
  learner_name = names(sl$cvRisk)[learner_index]

  # Set learner name as the subtitle if no subtitle was specified.
  if (is.null(subtitle)) {
    subtitle = paste("Best learner:", learner_name)
  }

  # Take the first row in case the best learner is duplicated for some random reason.
  best_row = auc_table[rownames(auc_table) == learner_index, , drop = FALSE]
  auc =  best_row$auc
  ci_upper = best_row$ci_upper
  ci_lower = best_row$ci_lower

  txt = paste0("AUC = ",
               sprintf(paste0("%0.", digits, "f"), round(auc, digits)),
               "\n95% CI = ", sprintf("%0.3f", round(ci_lower, digits)),
               " - ", sprintf("%0.3f", round(ci_upper, digits)))

  # ggplot version.
  print(ggplot2::qplot(1 - methods::slot(perf1, "x.values")[[1]],
                       methods::slot(perf1, "y.values")[[1]],
                       geom = "line",
                       main = title) +
          ggplot2::labs(subtitle = subtitle,
                        x = "False positive % (1 - specificity)",
                        y = "True positive % (sensitivity)") +
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
