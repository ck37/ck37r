library(ck37r)


# Note: when run as an automated check this will work, but manually it won't due to inst/extdata.
files = import_csvs("extdata")

names(files)
