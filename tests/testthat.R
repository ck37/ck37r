library(testthat)
library(ck37r)

test_check("ck37r", reporter = "check")

if (F) {
  # Run manually.
  test_package("ck37r")
  test_package("ck37r")
}
