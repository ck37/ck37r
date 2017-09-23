#' @title Create a dataset
#'
#' @description To be transferred into a separate package for Alan.
#'
#' @param data Dataframe with X and Y variables.
# TODO: support data.tables and tibbles.
#' @param varsX Vector of variables to use as predictors.
#' @param varY Variable name to use as the outcome.
#' @param config_code R code to create varsX and varY, which define the
#'   variables to use.
#' @param rename_vars named vector, where names are the original names and
#'   values are the new names.
#' @param remove_vars vector of strings for variables to remove from varsX
#' @param subset Code to further subset the data.
#' @param ylevel Target level of y if y is a factor variable.
#' @param primary_key Id variable, if any.
#' @param verbose If TRUE display extra output.
#'
#' @export
#
# TODO: add examples
create_dataset = function(data, varsX = "", varY = "",
                          config_code = NULL,
                          rename_vars = c(),
                          remove_vars = c(),
                          subset = "all",
                          ylevel = NULL,
                          primary_key = NULL,
                          verbose = F) {

  # This code would create varsX and varY if extra logic is needed.
  if (!is.null(config_code)) {
    eval(parse(text = config_code))
  }

  # Remove any leading or trailing spaces from varsX names.
  varsX = stringr::str_trim(varsX, side = "both")

  if (length(rename_vars) > 0) {
    # Only conduct the renaming if the original variable name is in varsX.
    good_renames = names(rename_vars) %in% varsX

    if (any(good_renames)) {
      varsX[match(names(rename_vars[good_renames]), varsX)] = rename_vars[good_renames]
      cat("Renamed variables:\n")
      print(rename_vars)
    }

    if (any(!good_renames))  {
      cat("Unfound rename vars:", paste(names(rename_vars[!good_renames]), sep=", "), "\n")
    }
  }

  # Check for duplication, esp. after renaming.
  duplicated_vars = duplicated(varsX)
  if (any(duplicated_vars)) {
    cat("Duplicated X vars:", paste(varsX[duplicated_vars], sep=", "), "\n")
    # Remove the duplicates to be cleaner.
    varsX = varsX[!duplicated_vars]
  }

  # Remove any variables that are specified.
  if (length(remove_vars) > 0) {

    good_removes = remove_vars %in% varsX
    if (any(good_removes)) {
      varsX = varsX[!varsX %in% remove_vars[good_removes]]
      cat("Removed variables:", paste(remove_vars[good_removes], sep=", "), "\n")
    }

    if (any(!good_removes)) {
      cat("Unfound remove vars:", paste(remove_vars[!good_removes], sep=", "), "\n")
    }
  }

  # Review which X variables are not in the dataframe, if any.
  x_in_data = varsX %in% names(data)

  # Search for similar variable names if any are missing.
  if (sum(!x_in_data) > 0) {
    cat("Could not find these variables in the dataframe:",
        paste(varsX[!x_in_data], sep = ", "), "\n")
    # Display similar variables.
    print(sapply(varsX[!x_in_data],
                 FUN = function(x) agrep(x, names(data),
                                         max.distance = list(sub = 0),
                                         value = T)))
  }

  # Review if the outcome variable is in the dataframe.
  found_y = varY %in% names(data)

  # If not, search for a similar variable.
  if (!found_y) {
    cat("Could not find Y in the dataframe. Similar variables:\n")
    print(agrep(varY, names(data), max.distance = list(sub = 0), value=T))
  }

  rm(found_y)

  # Create a custom outcome variable based on varY selector logic.
  # This have no real effect if varY is already just a variable name,
  # but would matter if varY was a string with executable R in it.
  # This assigns Y2 in the main function environment rather than inside with().
  Y2 = with(data, { eval(parse(text = varY)) })

  # Subset to the specified variables in the dataframe.
  data_subset = data[, varsX, drop = FALSE]
  dim(data_subset)

  # Save the primary key for each row as a vector.
  if (!is.null(primary_key)) {
    id = data[, primary_key]
  }

  if (subset != "all") {
    # Allow custom subset code that operates on the column names of dat.sht.

    # Evaluate custom subsetting code as though it were an R statement
    custom_subset = with(data, { eval(parse(text = subset)) })

    # Assume that we want to include any obs flagged as NA.
    custom_subset[is.na(custom_subset)] = TRUE

    cat("Reviewing our custom subset indicator.\n")
    print(table(custom_subset, useNA = "ifany"))

    # Restrict dataset to the subsetted observations.
    data_subset = data_subset[custom_subset,]

    # Review resulting dimensions.
    cat("Resulting data dimensions:", paste(dim(data_subset), sep = ", "), "\n")

    # Restrict outcomes to the subsetted observations.
    Y2 = Y2[custom_subset]

    # And update id vector.
    if (!is.null(primary_key)) {
      id = id[custom_subset]
    }
  }

  if (!is.null(ylevel)) {
    # If a ylevel was specified then the target outcome is an indicator for that level.
    Y = as.numeric(Y2 == ylevel)
  } else {
    # Otherwise just use the full Y2 as the target outcome.
    Y = Y2
  }

  # Restrict analysis to observations not missing the outcome variable.
  non_missing = !is.na(Y)
  cat("Reviewing missing values in Y. False = missing.\n")
  print(table(non_missing))
  data_subset = data_subset[non_missing, , drop = FALSE]
  Y = Y[non_missing]

  if (!is.null(primary_key)) {
    id = id[non_missing]
  } else {
    id = NULL
  }

  #if (verbose) {
  #  cat("Observations missing the outcome variable:",
  #      paste0(which(!non_missing), sep = ","), "\n")
  #}

  cat("Final dimensions of X:", paste(dim(data_subset), sep = ", "), "\n")

  # Make sure that our X and Y rows match.
  stopifnot(nrow(data_subset) == length(Y))

  # Make sure that our X and id rows match.
  if (!is.null(primary_key)) {
    stopifnot(nrow(data_subset) == length(id))
  }

  # Make sure Y is numeric and not a factor.
  if (class(Y) == "factor") {
    cat("Converting Y from a factor to numeric.\n")
    # Y should be 0/1
    newY = as.numeric(Y) - 1
    # Confirm that this conversion is correct before we update Y.
    print(table(newY, Y))
    Y = newY
  }

  results = list(X = data_subset, Y = Y, id = id)
  results
}
