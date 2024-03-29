% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prauc_table.CV.SuperLearner.R
\name{prauc_table.CV.SuperLearner}
\alias{prauc_table.CV.SuperLearner}
\title{Table of cross-validated PR-AUCs from CV.SuperLearner result}
\usage{
\method{prauc_table}{CV.SuperLearner}(
  x,
  y = x$Y,
  sort = TRUE,
  null_hypothesis = NULL,
  two_tailed = FALSE,
  lower.tail = TRUE,
  ...
)
}
\arguments{
\item{x}{CV.SuperLearner object}

\item{y}{Outcome vector, if not already added to CV.SL object.}

\item{sort}{Sort table by order of AUC.}

\item{null_hypothesis}{Not implemented yet}

\item{two_tailed}{Not implemented yet}

\item{lower.tail}{Not implemented yet}

\item{...}{Any additional unused arguments, due to the prauc_table generic.}
}
\value{
Dataframe table with PR-AUC and std dev.
}
\description{
Calculates cross-validated PR-AUC for each learner in the CV.SuperLearner. Also
calculates standard-error, confidence interval and p-value. Based on initial
code by Alan Hubbard.
}
\examples{
library(SuperLearner)
library(ck37r)

data(Boston, package = "MASS")

set.seed(1)
y = as.numeric(Boston$medv > 23)
cvsl = CV.SuperLearner(Y = y,
                       X = subset(Boston, select = -medv),
                       family = binomial(),
                       cvControl = list(V = 2, stratifyCV = TRUE),
                       SL.library = c("SL.mean", "SL.glm"))
prauc_table(cvsl, y = y)

}
\references{
Boyd, K., Eng, K. H., & Page, C. D. (2013). Area under the precision-recall
curve: point estimates and confidence intervals. In Joint European conference
on machine learning and knowledge discovery in databases (pp. 451-466).
Springer, Berlin, Heidelberg.

Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
Division of Biostatistics Working Paper Series. Paper 226.
http://biostats.bepress.com/ucbbiostat/paper266/

van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
Statistical Applications of Genetics and Molecular Biology, 6, article 25.
http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
}
\seealso{
\code{\link{cvsl_auc}} \code{\link{plot_roc.SuperLearner}}
   \code{\link[cvAUC]{ci.cvAUC}}
}
