library(ck37r)
library(testthat)

context("load all code")

print(getwd())

# If running via devtools::check() or test(), working dir will be tests/testthat.
if (dir.exists("R")) {
  load_all_code("R", verbose = T)
} else {
  load_all_code("../../R", verbose = T)
}
