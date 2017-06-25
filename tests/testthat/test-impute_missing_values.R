library(ck37r)
library(testthat)

context("Impute missing values")

# Load a test dataset.
data(PimaIndiansDiabetes2, package = "mlbench")

data = PimaIndiansDiabetes2

# Check for missing values.
colSums(is.na(data))

# Convert pregnant to a factor
data$pregnant = as.factor(data$pregnant)

# Add some missing values.
data$pregnant[1:3] = NA

# Convert mass to a character for testing purposes.
data$mass = as.character(data$mass)

# Impute missing data and add missingness indicators.
# Don't impute the outcome though.
# This will generate warnings due to data$visibility.
suppressWarnings({
  result = impute_missing_values(data, skip_vars = "diabetes", verbose = T)
})

# Confirm we have no missing data.
colSums(is.na(result$data))


#############
# K-nearest neighbors imputation

result2 = impute_missing_values(PimaIndiansDiabetes2, type = "knn",
                                skip_vars = "diabetes")

# Confirm we have no missing data.
colSums(is.na(result2$data))
