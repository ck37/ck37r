library(ck37r)
library(testthat)

context("se_auc")

test_that("se_auc matches the archived auctestr::se_auc()", {
  # Reference values computed with auctestr 1.0.0 (archived on CRAN).
  expect_equal(se_auc(0.75, 20, 200), 0.0649828274, tolerance = 1e-8)
  expect_equal(se_auc(0.75, 110, 110), 0.0328204841, tolerance = 1e-8)
  expect_equal(se_auc(0.75, 20, 20), 0.0778907202, tolerance = 1e-8)
})

test_that("se_auc matches the Hanley-McNeil formula computed by hand", {
  # With AUC = 0.5, Q1 = Q2 = 1/3, so
  # SE = sqrt((0.25 + 49 * (1/3 - 0.25) * 2) / (50 * 50)).
  expected = sqrt((0.25 + 49 * (1 / 3 - 0.25) * 2) / 2500)
  expect_equal(se_auc(0.5, 50, 50), expected)

  # Larger samples should give smaller standard errors.
  expect_lt(se_auc(0.8, 500, 500), se_auc(0.8, 50, 50))
})

test_that("auc_inference uses se_auc for its standard error and CI", {
  set.seed(1)
  true = rbinom(200, 1, 0.3)
  pred = true + rnorm(200)

  result = auc_inference(true, pred)
  expected_se = se_auc(result$auc, sum(true), sum(true == 0))

  expect_equal(result$se, expected_se)
  expect_equal(result$ci, result$auc + c(-1, 1) * qnorm(0.975) * expected_se)
  expect_true(result$auc > 0.5 && result$auc < 1)
})
