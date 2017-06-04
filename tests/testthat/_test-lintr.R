# This test file should be run manually.
# It cannot be run automatically because it will fail.

library(ck37r)
library(testthat)

if (requireNamespace("lintr", quietly = T)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}

# From https://github.com/jimhester/lintr#project-configuration
if (F) {
  library(magrittr)
  library(dplyr)
  lintr::lint_package() %>%
    as.data.frame %>%
    group_by(linter) %>%
    tally(sort = T) %$%
    sprintf("linters: with_defaults(\n    %s\n    NULL\n  )\n",
          paste0(linter, " = NULL, # ", n, collapse = "\n    ")) %>%
    cat(file = ".lintr-defaults")
}
