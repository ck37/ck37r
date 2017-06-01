library(ck37r)

# During devtools::test() working directory is tests/testthat.
if (dir.exists("../../inst/extdata")) {
  test_dir = "../../inst/extdata"
} else {
  # When run manually working directory will be main package directory.
  test_dir = "inst/extdata"
}

files = import_csvs(test_dir, verbose = T)

names(files)
