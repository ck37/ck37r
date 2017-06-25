#' @title Convert factors to indicator variables.
#'
#' @description Turns factors into indicator variables with reasonable names.
#'
#' Based on code by Taylor Terry from 2013.
#'
#' @param data .
#' @param predictors .
#' @param verbose TBD
#' @importFrom stats model.matrix
#' @export
factors_to_indicators =
  function(data, predictors = colnames(data), verbose = F) {

  # TODO: Check type of data and stop() early to save time.

  factor_names =
    names(which(sapply(data[, names(data) %in% predictors], is.factor)))

  cat(paste0("Converting factors (", length(factor_names), "): ",
             paste(factor_names, collapse = ", "), "\n"))

  all_factor_names = c()

  for (i in factor_names) {
    if (verbose) {
      cat("Converting", i, "from a factor to a matrix.\n")
    }

    # First, convert it again to a factor because this will drop unused levels.
    data[[i]] = factor(data[[i]])

    # If the factor has only one level we don't want to add it.
    # This should generally not be the case because we should have already
    # removed single-value (zero variation) columns.
    if (length(levels(data[[i]])) == 1) {
      if (verbose) {
        cat("Skipping", i, "because it has only 1 level.\n")
      }
      # Remove that column from the list of predictors.
      predictors = predictors[!predictors == i]
      data[[i]] = NULL
      next
    }

    # Convert field to a single-column data frame.
    # Here we do allow an intercept term in model.matrix, which results in one factor level
    # being left out and used as the reference field.

    tryCatch({
      # This can cause an error if a factor has only 1 value even if it has
      # multiple levels.
      # We allow an intercept column to be added so that 1 level serves as the
      # reference level.
      # TODO: record the reference level for use on prediction data.
      # (see Win-Vector blog on this topic)
      col_df = as.data.frame(model.matrix(~ factor(data[[i]])))
    }, error = function(e) {
      print(e)
    })

    # Now remove the intercept column.
    col_df[1] = NULL

    # Clean up indicator names.
    indicator_names = gsub(pattern = "factor.*\\)",
                           replacement = paste0(i, "_"), colnames(col_df))

    # Replace spaces and hyphens with underscores, and convert to lowercase.
    indicator_names = tolower(gsub(pattern = "[ -]", replacement = "_",
                                   indicator_names, perl = T))
    if (verbose) {
      cat(":", indicator_names, "\n")
    }
    colnames(col_df)  = indicator_names
    all_factor_names = c(all_factor_names, indicator_names)

    # This is the slow step, but it's still plenty fast for our purposes.
    data = cbind(data, col_df)

    # We want the deletion to happen after we've successfully cbound, in case of
    # error.
    data[[i]] = NULL

    rm(col_df)

    # Remove that column from the list of predictors.
    predictors = predictors[!predictors == i]
  }

  # Append factor names to the names of the predictors.
  predictors = c(predictors, all_factor_names)

  result = list(
    data = data,
    predictors = predictors,
    factor_names = all_factor_names
  )

  return(result)

}
