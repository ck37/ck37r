library(ck37r)

# Note: when run as an automated check this will work, but manually it won't due to inst/extdata.
files = import_csvs("extdata", verbose = T)

# This version is for testing manually.
if (F) {
  files = import_csvs("inst/extdata", verbose = T)
}

names(files)
