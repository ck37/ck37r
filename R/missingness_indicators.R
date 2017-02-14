#' @title Return matrix of missingness indicators for a dataframe or matrix.
#' @description Return matrix of missingness indicators for a dataframe or matrix.
#' @param data Dataframe or matrix to analyze for missingness.
#' @param prefix Name prefix for new indicator columns.
#' @param remove_constant Remove any indicators that are all 0 or all 1.
#' @param remove_collinear Remove any indicators that are collinear with each other.
#' @param verbose If TRUE, print additional information.
#' @return Matrix of missingness indicators, with any constant indicators removed.
#' @export
missingness_indicators = function(data, prefix = "miss_",
                                  remove_constant = T,
                                  remove_collinear = T,
                                  verbose = F) {
  # Create indicators.
  indicators = sapply(data, FUN = function(col) as.numeric(is.na(col)) )

  colnames(indicators) = paste0(prefix, colnames(data))

  # Remove any indicators that are all 0 or all 1.
  if (remove_constant) {
    col_means = colMeans(indicators)
    if (verbose) {
      num_removed = sum(col_means %in% c(0, 1))
      if (num_removed > 0) {
        cat("Removing", num_removed, "indicators that are constant.\n")
      }
    }
    # Set drop = F to ensure we don't convert to a vector when
    # only one column remains.
    indicators = indicators[, !col_means %in% c(0, 1), drop = F]
  }

  # Remove any collinear indicators.
  if (remove_collinear) {

    ######################
    # http://stackoverflow.com/questions/12304963/using-eigenvalues-to-test-for-singularity-identifying-collinear-columns

    # Compute t(x) %*% x
    crossprod = crossprod(indicators)
    eigen_decomp = eigen(crossprod)

    # Check for perfectly collinear columns in our data frame.
    # TODO: clean up code syntax a bit more.
    extract_collinearity <- function(eigen_decomp) {
      # Split eigenvector matrix into a list, by columns.
      evecs = split(zapsmall(eigen_decomp$vectors), col(eigen_decomp$vectors))

      # For non-zero eigenvalues, list non-zero evec components
      mapply(function(eigenvalue, vec) {
        if (eigenvalue != 0) {
          # No collinear columns for this eigenvalue.
          NULL
        } else {
          # We have collinearity so return non-zero indices.
          which(vec != 0)
        }
      }, zapsmall(eigen_decomp$values), evecs)
    }

    collinear_analysis = extract_collinearity(eigen_decomp)

    # Vector of collinear columns to keep.
    keep_columns = NULL

    # Vector of collinear columsn to remve.
    remove_columns = NULL

    for (index in 1:length(collinear_analysis)) {
      indices = collinear_analysis[[index]]
      if (!is.null(indices)) {
        already_kept = indices[indices %in% keep_columns]
        if (length(already_kept) > 0) {
          # Append any additional columns to the remove.
          remove_columns = c(remove_columns, indices[!indices %in% keep_columns])
        } else {
          # Just keep the first column and remove the remainder.
          # NOTE: this may keep a column that is already planned to be removed.
          # Do we want to prevent that?

          # Restrict to columns that we don't already want to remove.
          indices = indices[!indices %in% remove_columns]

          if (length(indices) > 0) {
            keep_columns = c(keep_columns, indices[1])
            if (verbose) {
              cat("Keeping:", colnames(indicators)[indices[1]], "\n")
            }
            remove_columns = c(remove_columns, indices[-1])
          }
        }

      }
    }

    if (length(remove_columns) > 0) {
      if (verbose) {
        cat("Removing", length(remove_columns), "indicators due to collinearity:\n")
        cat(paste0(colnames(indicators)[remove_columns], collapse = ", "), "\n")
      }

      # Make sure we don't switch to a vector if only 1 column remains.
      indicators = indicators[, -remove_columns, drop = F]
    }
  }

  return(indicators)
}
