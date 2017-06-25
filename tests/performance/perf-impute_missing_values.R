library(ck37r)
library(microbenchmark)

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


#####################3
# Initial function setup.
skip_vars = "diabetes"
type = "standard"
verbose = T
add_indicators = T
prefix = "miss_"

######################
# Code inside of impute_missing_values()

# Loop over each feature.
missing_indicators = NULL

# Make a copy to store the imputed dataframe.
new_data = data

# Only check variables that we don't want to skip.
vars = !colnames(data) %in% skip_vars

# List of results to populate.
# Save our configuration first.
results = list(type = type,
               add_indicators = add_indicators,
               skip_vars = skip_vars,
               prefix = prefix)

# Identify columns with any NAs.
# We apply skip_vars within the function so that which() indices are correct.
any_nas = which(sapply(colnames(data),
                       function(col) !col %in% skip_vars && anyNA(data[, col])))
any_nas

# List to save the imputation values used.
# We need a list because it can contain numerics and factors.
impute_values = vector("list", sum(vars))

# Copy variable names into the imputed values vector.
names(impute_values) = colnames(data[vars])

#####################################
# Benchmark 1.
# Calculate number of NAs in advance.

result = microbenchmark::microbenchmark(
  # BM Option 1.
  { sum_nas = sapply(any_nas, function(i) sum(is.na(data[[i]]))) },
  # BM Option 2.
  { sum_nas = colSums(is.na(data[, any_nas, drop = F])) },
  times = 10000L
)

# Option 1 is 14% - 29% faster, so use that.
result
