#' Standard error of the AUC
#'
#' Computes the standard error of an AUC estimate using its equivalence to the
#' Wilcoxon statistic (Hanley & McNeil 1982). Adapted from
#' \code{auctestr::se_auc()} (MIT license, Copyright 2017 Josh Gardner), which
#' has been archived on CRAN.
#'
#' @param auc AUC estimate (numeric).
#' @param n_p Number of positive cases.
#' @param n_n Number of negative cases.
#'
#' @return Standard error of the AUC.
#'
#' @references Hanley and McNeil, The meaning and use of the area under a
#'   receiver operating characteristic (ROC) curve. Radiology (1982) 143 (1)
#'   pp. 29-36.
#'
#' @examples
#' se_auc(0.75, 20, 200)
#' # Standard error decreases when classes are more balanced.
#' se_auc(0.75, 110, 110)
#' # Standard error increases when sample size shrinks.
#' se_auc(0.75, 20, 20)
#'
#' @export
se_auc = function(auc, n_p, n_n) {
  d_p = (n_p - 1) * ((auc / (2 - auc)) - auc^2)
  d_n = (n_n - 1) * ((2 * auc^2) / (1 + auc) - auc^2)
  sqrt((auc * (1 - auc) + d_p + d_n) / (n_p * n_n))
}
