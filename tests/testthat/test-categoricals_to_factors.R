library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "mlbench")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

data(Ozone, package = "mlbench")

data = Ozone

# Add column names.
names(data) =
  tolower(c("Month", "Day_of_month", "Day_of_week", "ozone_reading",
            "pressure_height", "Wind_speed", "Humidity", "Temperature_Sandburg",
            "Temperature_ElMonte", "Inversion_base_height", "Pressure_gradient",
            "Inversion_temperature", "Visibility"))
str(data)

# Convert factors to numerics
categoricals = names(which(sapply(data, is.factor)))
categoricals

data[, categoricals] = sapply(data[, categoricals], as.numeric)

str(data[, categoricals])

# Convert first categorical to a factor in advance, for testing purposes.
data[, categoricals[1]] = as.factor(data[, categoricals[1]])

# Add a non-existent column for testing purposes.
categoricals = c(categoricals, "blah123blah")

# Now convert back to factors.
new = categoricals_to_factors(data, categoricals, verbose = T)

# Skip the random name we added at the end of the vector.
str(new[, categoricals[-length(categoricals)]])
