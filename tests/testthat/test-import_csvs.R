library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

# During devtools::test() working directory is tests/testthat.
if (dir.exists("../../inst/extdata")) {
  test_dir = "../../inst/extdata"
} else if (dir.exists("../../extdata")) {
  # After package is installed inst/extdata will be moved to extdata.
  test_dir = "../../extdata"
} else {
  # When run manually working directory will be main package directory.
  test_dir = "inst/extdata"
}

files = import_csvs(test_dir, verbose = T)

names(files)

# Go up one directory, which should be empty.
# This will generate a warning.
suppressWarnings({
  files = import_csvs(paste0(test_dir, "/../"), verbose = T, recursive = F)
})

names(files)
