#' @title Convert factors to indicator variables.
#'
#' @description Turns factors into indicator variables with reasonable names.
#'
#' Supports parallelization using \code{future} plans.
#'
#' Based on code by Taylor Terry from 2013.
#'
#' @param data .
#' @param predictors .
#' @param max_levels If a factor contains more than this many levels, issue
#' a warning and don't convert it to indicators.
#' @param verbose TBD
#' @importFrom stats model.matrix.lm
#' @importFrom future.apply future_lapply
#' @export
factors_to_indicators =
  function(data, predictors = colnames(data), max_levels = 200L, verbose = FALSE) {

  # TODO: Check type of data and stop() early to save time.

  factor_names =
    names(which(sapply(data[, names(data) %in% predictors], is.factor)))

  cat(paste0("Converting factors (", length(factor_names), "): ",
             paste(factor_names, collapse = ", "), "\n"))

  all_factor_names = c()

  # TODO: run in parallel, compile as a list, then do a single cbind at the end.
  # for (i in factor_names) {
  results = future_lapply(factor_names, future.seed = TRUE, function(factor_i) {

    # First, convert it again to a factor because this will drop unused levels.
    factor_data = factor(data[[factor_i]])

    # Show user how many factor levels there are.
    total_levels = length(levels(factor_data))

    if (verbose) {
      cat("Converting", factor_i, "from a factor to a matrix",
          paste0("(", total_levels, " levels).\n"))
    }

    if (total_levels > max_levels) {
      msg = paste(factor_i, "has too many levels",
              paste0("(", total_levels, "),"), "skipping conversion.")
      if (verbose) cat(msg, "\n")
      warning(msg)
      return(NULL)
    }

    # If the factor has only one level we don't want to add it.
    # This should generally not be the case because we should have already
    # removed single-value (zero variation) columns.
    if (total_levels == 1L) {
      if (verbose) {
        cat("Skipping", factor_i, "because it has only 1 level.\n")
      }
      return(NULL)
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
      # TODO: use most common level as the reference level.
      # Convert to integers rather than numerics, for possible memory savings.
      # We use model.matrix.lm() and na.pass so that factors with NAs don't
      # mess up the observation count.
      mat = model.matrix.lm(~ factor(factor_data), na.action = "na.pass")
      # Convert to integer to save memory.
      mode(mat) = "integer"

      col_df = mat

      #col_df = as.data.frame(mat)
    }, error = function(e) {
      print(e)
    })

    # Now remove the intercept column.
    #col_df[1] = NULL
    col_df = col_df[, -1, drop = FALSE]

    # Clean up indicator names.
    indicator_names = gsub(pattern = "factor.*\\)",
                           replacement = paste0(factor_i, "_"), colnames(col_df))

    # Replace spaces and hyphens with underscores, and convert to lowercase.
    indicator_names = tolower(gsub(pattern = "[ -]", replacement = "_",
                                   indicator_names, perl = TRUE))
    if (verbose) {
      cat(":", indicator_names, "\n")
    }
    colnames(col_df)  = indicator_names

    # Return the dataframe for this factor.
    col_df
  })

  # Remove columns from the list of predictors.
  # TODO: don't remove columns that were skipped due to having too
  # many levels.
  predictors = setdiff(predictors, factor_names)

  # Remove original factor columns from the dataframe.
  data = subset(data, select = setdiff(colnames(data), factor_names))
  #data = data[ , -c(factor_names), with = FALSE]

  #browser()
  if (verbose) {
    cat("Combining factor matrices into a data frame.\n")
  }

  # cbind all new columns into data frame; skip any elements that are NULL.
  data = do.call(cbind, c(list(data), results[!sapply(results, is.null)]))

  # Compile new factor names into a new vector (all_factor_names)
  all_factor_names = sapply(results, colnames)

  # Append factor names to the names of the predictors.
  predictors = c(predictors, all_factor_names)

  result = list(
    data = data,
    predictors = predictors,
    factor_names = all_factor_names
  )

  return(result)

}
