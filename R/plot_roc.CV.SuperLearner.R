#' @title Plot a ROC curve from cross-validated AUC from CV.SuperLearner
#' @description
#' Based on initial code by Alan Hubbard.
#'
#' @param x CV.SuperLearner object
#' @param y Outcome vector if not already included in the SL object.
#' @param learner Which learner to plot - defaults to highest AUC learner.
#' @param title Title to use in the plot.
#' @param subtitle Optional plot subtitle.
#' @param digits Digits to use when rounding AUC and CI for plot.
#' @param show_plot If TRUE print the ggplot, otherwise just return in list.
#' @param ... Any additional unused arguments, due to the auc_table generic.
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
plot_roc.CV.SuperLearner =
  function(x, y = x$Y,
           learner = NULL,
           title = "CV-SuperLearner cross-validated ROC",
           subtitle = NULL,
           digits = 4,
           show_plot = TRUE,
           ...) {
  # Change to a better object name.
  cvsl = x

  auc_table = ck37r::auc_table(cvsl, y)

  cvsl_table = summary(cvsl)$Table

  # Choose the learner with the highest AUC.
  # TODO: support passing in strings "SuperLearner" or "DiscreteSL" rather than
  # an index.
  if (is.null(learner)) {
    # Note: multiple learners may have the best AUC.
    best_auc = max(auc_table$auc, na.rm = TRUE)

    # First extract the learner name, then later get the index.
    learner_name = rownames(auc_table)[auc_table$auc == best_auc]

    if (length(learner_name) > 1) {
      # TODO: sort by SE first, so that we take the learner with the lower SE.
      # (Although generally any AUC ties will have the same SE.)

      # If there are multiple winners and SuperLearner is in there, use SL.
      # Otherwise use DiscreteSL, otherwise use the first learner name.
      if ("SuperLearner" %in% learner_name) {
        learner_name = "SuperLearner"
      } else if ("DiscreteSL" %in% learner_name) {
        learner_name = "DiscreteSL"
      } else {
        learner_name = learner_name[1]
      }
    }

    # Extract the original learner index based on learner name in the AUC table.
    learner = which(cvsl_table$Algorithm == learner_name)
  } else {
    learner_name = as.character(cvsl_table$Algorithm[learner])
  }

  if (learner_name == "SuperLearner") {
    preds = cvsl$SL.predict
  } else if (learner_name == "DiscreteSL") {
    preds = cvsl$discreteSL.predict
  } else {
    # library.predict contains only the raw learners, not SL or DiscreteSL.
    preds = cvsl$library.predict[, learner - 2]
  }

  # Set learner name as the subtitle if no subtitle was specified.
  if (is.null(subtitle)) {
    subtitle = paste("Best learner:", learner_name)
  }

  auc = auc_table[learner_name, "auc"]
  ci_upper = auc_table[learner_name, "ci_upper"]
  ci_lower = auc_table[learner_name, "ci_lower"]

  txt = paste0("AUC = ",
               sprintf(paste0("%0.", digits, "f"), round(auc, digits)),
               "\n95% CI = ", sprintf("%0.3f", round(ci_lower, digits)),
               " - ", sprintf("%0.3f", round(ci_upper, digits)))

  pred = ROCR::prediction(preds, y)
  perf1 = ROCR::performance(pred, "sens", "spec")

  # ggplot version.
  p = ggplot2::qplot(1 - methods::slot(perf1, "x.values")[[1]],
                     methods::slot(perf1, "y.values")[[1]],
                     geom = "line",
                     main = title) +
          ggplot2::labs(subtitle = subtitle,
                        x = "False positive % (1 - specificity)",
                        y = "True positive % (sensitivity)") +
          ggplot2::theme_bw() +
          ggplot2::annotate("text", x = 0.63, y = 0.15, label = txt, size = 6) +
          ggplot2::annotate("segment", x = 0, xend = 1, y = 0, yend = 1)

  if (show_plot) {
    print(p)
  }

  # Return AUC, AUC CI, and ggplot object.
  results = list(auc = auc,
                 auc_se = auc_table[learner_name, "se"],
                 auc_ci = c(ci_lower, ci_upper),
                 plot = p)

  # Return results invisibly.
  invisible(results)
}
