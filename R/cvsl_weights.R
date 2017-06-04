#' @title Create a table of meta-weights from a CV.SuperLearner
#'
#' @description Returns summary statistics (mean, sd, min, max) on the
#'   distribution of the weights assigned to each learner across SuperLearner
#'   ensembles. This makes it easier to understand the stochastic nature of the
#'   SL learner weights and to see how often certain learners are used. This
#'   function may eventually be moved into the SuperLearner package.
#'
#' @param cvsl CV.SuperLearner result object
#' @param sort  If TRUE sort rows (learners) in descending order by mean weight.
#' @param nonzero Restrict to learners with a non-zero mean weight.
#' @param clean_names Remove "SL." from the front and "_All" from the end of
#'   learner names.
#' @param rank Adding the learner rank to the table.
#' @param digits Number of digits to round the results. Set to NULL to disable.
#'
#' @return Table in data frame form with each learner's mean, sd, min, and max
#'   meta-weight in the ensemble of each learner.
#'
#' @examples
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
#' cvsl_weights(cvsl)
#'
#' @references
#'
#' Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
#' Division of Biostatistics Working Paper Series. Paper 226.
#' http://biostats.bepress.com/ucbbiostat/paper266/
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#' http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
#'
#' @importFrom stats coef sd
#' @export
cvsl_weights = function(cvsl,
                        sort = T,
                        nonzero = F,
                        clean_names = T,
                        rank = T,
                        digits = 5) {
  meta_weights = coef(cvsl)

  # Calculate summary statistics.
  means = colMeans(meta_weights)
  sds = apply(meta_weights, MARGIN = 2, FUN = sd)
  mins = apply(meta_weights, MARGIN = 2, FUN = min)
  maxs = apply(meta_weights, MARGIN = 2, FUN = max)

  # Combine the stats into a single matrix.
  sl_stats = cbind("Mean" = means,
                   "SD" = sds,
                   "Min" = mins,
                   "Max" = maxs)

  # Convert from SD to SE? (SD / sqrt(n))?

  # Sort in descending order of mean weight.
  if (sort) {
    sl_stats = sl_stats[order(sl_stats[, 1], decreasing = T), , drop = F]
  }

  # Restrict to models with non-zero mean weight.
  if (nonzero) {
    sl_stats = sl_stats[sl_stats[,  1] > 0, , drop = F]
  }

  # Round the numerics.
  if (!is.null(digits)) {
    sl_stats = apply(sl_stats, MARGIN = c(1, 2), FUN = round, digits = digits)
  }

  # Move variable name into a real column.
  # Here we need to convert to a dataframe since we will have a string column.
  sl_stats = data.frame(Learner = rownames(sl_stats), sl_stats,
                        # Don't mess up the "mean(weight)" column name.
                        # (Not needed anymore, but still a good setting.)
                        check.names = F)
  rownames(sl_stats) = NULL

  # Remove extra cruft from rownames so table is prettier.
  if (clean_names) {
    sl_stats[, 1] = gsub("_All$", "", sl_stats[, 1])
    sl_stats[, 1] = gsub("^SL.", "", sl_stats[, 1])
  }

  # Add rank so we know each model's rank.
  if (rank) {
    sl_stats = cbind("#" = 1:nrow(sl_stats), sl_stats)
  }

  sl_stats
}
