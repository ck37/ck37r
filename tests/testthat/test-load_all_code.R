library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

context("load all code")

print(getwd())

# If running via devtools::check() or test(), working dir will be tests/testthat.
if (dir.exists("R")) {
  # Do within a throwaway env so that we don't load separate copies of the functions.
  load_all_code("R", verbose = T, envir = new.env())
} else {
  # Do within a throwaway env so that we don't load separate copies of the functions.
  load_all_code("../../R", verbose = T, envir = new.env())
}
