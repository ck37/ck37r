#' Summary table of variables
#'
#' Generate a summary table of variables in a dataframe.
#'
#' @param df Dataframe of variables
#' @param vars Variable names to analyze in the dataframe
#' @param groups TBD
#' @param integers Explicitly note integer-valued variables
#' @param ordinal Explicitly note ordinal variables
#'
#' @importFrom magritter %>%
#' @export
summarize_vars =
  function(df,
           covars,
           few_uniq_vals = 10L,
           groups = NULL,
           integers = NULL,
           ordinal = NULL) {

    var_df = data.frame(matrix(nrow = length(covars),
                               ncol = 13L))
    names(var_df) = c("var", "group", "class", "type",
                      "uniq_vals", "mode", "mean", "median",
                      "min", "pctile_0.1", "max", "pctile_99.9", "missingness")


    var_df$var = covars
    var_df$class = sapply(df[, covars], class)
    var_df$mode = sapply(df[, covars], function(var) ck37r::Mode(var)[1])

    # Don't count NAs as a unique value.
    var_df$uniq_vals = sapply(df[, covars], function(var) length(setdiff(unique(var), NA)))

    summary_stats = lapply(df[, covars], function(var) {
      summ = summary(var)
      var_cdf = ecdf(var)
      summ["pctile_99.9"] = quantile(var_cdf, 0.999)
      summ["pctile_0.1"] = quantile(var_cdf, 0.001)

      summ
    })

    var_df$mean = sapply(summary_stats, `[`, "Mean")
    var_df$min = sapply(summary_stats, `[`, "Min.")
    var_df$max = sapply(summary_stats, `[`, "Max.")
    var_df$median = sapply(summary_stats, `[`, "Median")

    var_df$pctile_99.9 = sapply(summary_stats, `[`, "pctile_99.9")
    var_df$pctile_0.1 = sapply(summary_stats, `[`, "pctile_0.1")

    var_df$missingness = colMeans(is.na(df[, covars]))

    # TODO: support no groups.
    for (group in names(groups)) {
      var_df$group[var_df$var %in% groups[[group]]] = group
    }

    # Factor variables are category.
    var_df$type[var_df$class == "factor"] = "categorical"

    # Programatically identify binary vars.
    binary_vars = var_df$class %in% c("numeric", "integer", "boolean") &
      var_df$min == 0 & var_df$max == 1 & var_df$uniq_vals == 2

    # Recode binary vars to integers.
    for (binary_var in vars$covariates[binary_vars]) {
      df[[binary_var]] = as.integer(df[[binary_var]])
    }

    # Update type
    var_df$type[binary_vars] = "binary"

    for (integer_var in integers) {
      df[[integer_var]] = as.integer(df[[integer_var]])
    }

    # These are all positive integers
    var_df$type[var_df$var %in% integers] = "pos. int."

    for (ordinal_var in ordinal) {
      df[[ordinal_var]] = as.integer(df[[ordinal_var]])
    }

    # These are all nonnegative integers
    var_df$type[var_df$var %in% ordinal] = "ordinal int."

    # Update class
    var_df$class = sapply(df[, covars], class)

    # Review numerics with few unique vals.
    few_unique_vals = var_df$class == "numeric" & var_df$uniq_vals <= few_uniq_vals

    for (var in var_df$var[few_unique_vals]) {
      cat("Var:", var, "\n")
      print(table(df[[var]], useNA = "ifany"))
    }

    # Make remaining numeric vars continuous type
    var_df$type[var_df$class == "numeric" & is.na(var_df$type)] = "continuous"

    # Sort by group and then variable name.
    var_df = var_df %>% dplyr::arrange(group, var) %>% as.data.frame()

    result = list(data = df,
                  table = var_df)

    result
  }
