library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "mlbench")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

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

# Add a column that is all NAs.
data$all_nas = NA
str(data)

# Impute missing data and add missingness indicators.
# Don't impute the outcome though.
# This will generate warnings due to data$mass.
suppressWarnings({
  result = impute_missing_values(data, skip_vars = "diabetes", verbose = TRUE)
})

# Confirm we have no missing data.
colSums(is.na(result$data))

# Test with no skip_vars.
test_that("Test that all_vars = T works, when skip_vars is NULL", {
  suppressWarnings({
    result = impute_missing_values(data, verbose = TRUE, all_vars = TRUE)
  })

  # Confirm that "diabetes" is in the impute_info even though it has no missingness.
  expect_true("diabetes" %in% names(result$impute_values))
})


# Test all_vars = T
suppressWarnings({
  result = impute_missing_values(data, skip_vars = "diabetes", verbose = TRUE,
                                 all_vars = TRUE)
})

# Test with pre-specifying values.
suppressWarnings({
  result2 = impute_missing_values(data, skip_vars = "diabetes", verbose = TRUE,
                                 all_vars = TRUE, values = result$impute_values)
})


#############
# K-nearest neighbors imputation

result2 = impute_missing_values(PimaIndiansDiabetes2, type = "knn",
                                skip_vars = "diabetes")

# Confirm we have no missing data.
colSums(is.na(result2$data))

#############
# GLRM imputation

#result2 = impute_missing_values(data, type = "glrm",
result2 = impute_missing_values(PimaIndiansDiabetes2, type = "glrm",
                                skip_vars = "diabetes", verbose = FALSE)

# Confirm we have no missing data.
colSums(is.na(result2$data))

# Explicitly shut down h2o to see if this helps appveyor build to not freeze.
h2o::h2o.shutdown(prompt = FALSE)
