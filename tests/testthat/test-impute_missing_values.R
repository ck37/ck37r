library(ck37r)
library(testthat)

context("Impute missing values")

# Load a test dataset.
data(PimaIndiansDiabetes2, package = "mlbench")

data = PimaIndiansDiabetes2

# Check for missing values.
colSums(is.na(data))

# Convert day_of_week to a factor
data$day_of_week = as.factor(data$day_of_week)

# Add some missing values.
data$day_of_week[1:3] = NA

# Convert visibility to a character for testing purposes.
data$visibility = as.character(data$visibility)
# Add include some missingness.
data$visibility[5:10] = NA

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
