library(ck37r)
library(testthat)

# During devtools::test() working directory is tests/testthat.
if (dir.exists("../../inst/extdata")) {
  test_dir = "../../inst/extdata"
} else {
  # When run manually working directory will be main package directory.
  test_dir = "inst/extdata"
}

files = import_csvs(test_dir, verbose = T)

names(files)

# Go up one directory, which should be empty.
# This will generate a warning.
suppressWarnings({
  files = import_csvs(paste0(test_dir, "../"), verbose = T, recursive = F)
})

names(files)
