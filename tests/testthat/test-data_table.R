library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "mlbench")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("data.table inputs")

# The helpers index their input with data.frame syntax, such as
# data[, cols, drop = FALSE]. data.table applies that syntax to a data.table
# only when the calling package does not import data.table, so each helper
# should give the same result for a data.table as for a data.frame.

data(SynthDiabetes2, package = "mlbench")
df = SynthDiabetes2
df$pregnant = as.factor(df$pregnant)
df$pregnant[1:3] = NA
dt = data.table::as.data.table(df)

test_that("missingness_indicators() handles a data.table", {
  ind_df = missingness_indicators(df, skip_vars = "diabetes")
  ind_dt = missingness_indicators(dt, skip_vars = "diabetes")
  expect_gt(ncol(ind_df), 0)
  expect_equal(ind_dt, ind_df)
})

test_that("impute_missing_values() handles a data.table", {
  res_df = impute_missing_values(df, skip_vars = "diabetes")
  res_dt = impute_missing_values(dt, skip_vars = "diabetes")
  expect_false(anyNA(res_dt$data))
  expect_true(data.table::is.data.table(res_dt$data))
  expect_equal(as.data.frame(res_dt$data), res_df$data)
  expect_equal(res_dt$impute_values, res_df$impute_values)
  expect_gt(length(res_dt$indicators_added), 0)
  expect_equal(res_dt$indicators_added, res_df$indicators_added)

  # Imputation values estimated on other data.
  new_df = impute_missing_values(df, skip_vars = "diabetes",
                                 values = res_df$impute_values)
  new_dt = impute_missing_values(dt, skip_vars = "diabetes",
                                 values = res_df$impute_values)
  expect_equal(as.data.frame(new_dt$data), new_df$data)
})

test_that("factors_to_indicators() handles a data.table", {
  df2 = SynthDiabetes2
  df2$pregnant = as.factor(df2$pregnant)
  dt2 = data.table::as.data.table(df2)
  res_df = factors_to_indicators(df2, predictors = "pregnant")
  res_dt = factors_to_indicators(dt2, predictors = "pregnant")
  expect_equal(res_dt$factor_vars, "pregnant")
  expect_equal(as.data.frame(res_dt$data), res_df$data)
  expect_equal(res_dt$predictors, res_df$predictors)

  res_dt2 = factors_to_indicators(dt2, predictors = "pregnant",
                                  data.table = TRUE)
  expect_true(data.table::is.data.table(res_dt2$data))
  expect_equal(as.data.frame(res_dt2$data), res_df$data)
})
