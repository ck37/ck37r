library(ck37r)
library(testthat)

context("Missingness indicators")

# Load a test dataset.
data(PimaIndiansDiabetes2, package = "mlbench")

# Check for missing values.
colSums(is.na(PimaIndiansDiabetes2))

# Generate missingness indicators; skip outcome variable.
indicators = missingness_indicators(PimaIndiansDiabetes2,
                                    skip_vars = "diabetes", verbose = T)

# Check missingness.
colSums(indicators)
