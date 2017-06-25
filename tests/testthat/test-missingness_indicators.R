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

# Test with a single-column dataframe (drop = F issue).
# This also tests the case when no missingness indicators are needed.
data = PimaIndiansDiabetes2

# 1 column, no missingness data.
indicators = missingness_indicators(data[, 1, drop = F], verbose = T)
# 1 column, has missingness.
indicators = missingness_indicators(data[, 2, drop = F], verbose = T)

# Test removal of collinear columns.
# Here we add glucose a second time to the end of the dataframe.
indicators = missingness_indicators(cbind(data, glucose2 = data[, 2]),
                                    verbose = T)

# Test removal of constant columns.
data[, 1] = NA
indicators = missingness_indicators(data, verbose = T)

